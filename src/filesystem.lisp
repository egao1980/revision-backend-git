(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; REVISION-FILESYSTEM — a read-only cl-stack-pathlib filesystem over the
;;; tree of one commit, so every pathlib tool (read-text, iterdir, walk,
;;; glob, grep, stat, content-hash …) reads any revision without a checkout.
;;;
;;; Paths are POSIX, rooted at "/" (the commit's top-level tree). Listings
;;; come from `ls-tree -z -l`, contents from the store's cat-file batch
;;; process; nothing is cached across stores, and the filesystem is pinned
;;; to the commit id resolved when it was made (moving a ref later does not
;;; change what it reads).
;;;
;;;   fs-exists-p / fs-file-p / fs-directory-p   tree entries; a symlink entry
;;;                                              is a file and FS-SYMLINK-P
;;;   fs-symlink-p / fs-read-symlink             mode 120000; the target as a
;;;                                              pathname (not followed)
;;;   fs-executable-p                            mode 100755
;;;   fs-readable-p                              exists; fs-writable-p → NIL
;;;   fs-file-size                               blob size (ls-tree -l)
;;;   fs-last-modified                           the commit's committer date,
;;;                                              universal time, every entry
;;;   fs-iterdir                                 children in git tree order
;;;   fs-read-bytes                              exact blob bytes
;;;   fs-walk / fs-glob                          as zip-filesystem (IGNORE too)
;;;   fs-stat                                    kind :FILE / :DIRECTORY
;;;                                              (:SYMLINK without following),
;;;                                              mode #o644 / #o755, mtime as
;;;                                              fs-last-modified
;;;   fs-parse / fs-join / fs-parent / fs-name / fs-absolute / fs-resolve /
;;;   fs-normpath / fs-expanduser / fs-same-p    POSIX, like zip-filesystem
;;;   every mutating GF, fs-make-temp, fs-as-uri → PATHLIB:UNSUPPORTED-OPERATION
;;;   a missing path                             → PATHLIB:PATH-NOT-FOUND
;;;   (read-bytes, file-size, iterdir, stat; with pathlib's restarts where
;;;   zip-filesystem offers them)
;;; Submodule (gitlink) entries are directories with no children.
;;; H2a GFs (fs-read-range, fs-read-lines, fs-open, fs-content-hash,
;;; fs-grep) may use pathlib's portable defaults.
;;; ------------------------------------------------------------------

(defclass revision-filesystem (pathlib:filesystem)
  ((store :initarg :store :reader revision-filesystem-store)
   (revision :initarg :revision :reader revision-filesystem-revision)
   (mtime :initarg :mtime :reader %fs-mtime
          :documentation "The commit's committer date, universal time.")
   (entries :initform nil :accessor %fs-entries)
   (children :initform nil :accessor %fs-children)
   (lock :initform (bt2:make-lock :name "revision-filesystem") :reader %fs-lock)
   (paths :initform (pathlib:make-memory-filesystem :cwd "/" :home "/")
          :reader %fs-paths
          :documentation "Private POSIX filesystem whose pure path GFs this one uses."))
  (:default-initargs :name "git-revision")
  (:documentation "Read-only CL-STACK-PATHLIB filesystem over one commit's tree.
REVISION-FILESYSTEM-REVISION is the commit id (a string)."))

(defun revision-filesystem-p (x)
  "T when X is a REVISION-FILESYSTEM."
  (typep x 'revision-filesystem))

(defun make-revision-filesystem (store designator)
  "→ REVISION-FILESYSTEM over the tree of the commit DESIGNATOR names in STORE
(any designator RESOLVE-REVISION takes). Signals what RESOLVE-REVISION
signals."
  (let ((revision (rev:resolve-revision store designator)))
    (make-instance 'revision-filesystem
                   :store store
                   :revision (rev:revision-id revision)
                   :mtime (+ (dt:instant-seconds (rev:revision-recorded-at revision))
                             (encode-universal-time 0 0 0 1 1 1970 0)))))

(defun revision-path (store designator &optional (path "/"))
  "→ CL-STACK-PATHLIB:PATH for PATH (a POSIX string, default \"/\") on a fresh
REVISION-FILESYSTEM of DESIGNATOR in STORE."
  (let ((fs (make-revision-filesystem store designator)))
    (pathlib:make-path (pathlib:fs-parse fs path) :filesystem fs)))

;;; ------------------------------------------------------------------
;;; The listing: one `ls-tree -r -t -z -l` per filesystem, on first use.
;;; ENTRIES maps "/a/b" → plist (:mode :type :id :size) ("/" is the root
;;; tree); CHILDREN maps a directory key → child names in git tree order.
;;; ------------------------------------------------------------------

(defun %fs-load (fs)
  (bt2:with-lock-held ((%fs-lock fs))
    (unless (%fs-entries fs)
      (let ((entries (make-hash-table :test 'equal))
            (children (make-hash-table :test 'equal)))
        (setf (gethash "/" entries) (list :mode #o40000 :type :tree)
              (gethash "/" children) '())
        (dolist (e (parse-ls-tree
                    (%git (revision-filesystem-store fs)
                          (list "ls-tree" "-r" "-t" "-z" "-l" (revision-filesystem-revision fs)))))
          (let* ((key (concatenate 'string "/" (getf e :path)))
                 (slash (position #\/ key :from-end t))
                 (parent (if (zerop slash) "/" (subseq key 0 slash))))
            (setf (gethash key entries) e)
            (push (subseq key (1+ slash)) (gethash parent children))
            (unless (eq (getf e :type) :blob)
              (setf (gethash key children) (or (gethash key children) '())))))
        (maphash (lambda (k v) (setf (gethash k children) (reverse v))) children)
        (setf (%fs-children fs) children
              (%fs-entries fs) entries))))
  fs)

(defun %posix-of (pathname)
  "Absolute POSIX string of the absolute, normalized PATHNAME (no trailing /)."
  (let* ((dir (pathname-directory pathname))
         (comps (and (consp dir) (remove-if-not #'stringp (rest dir))))
         (name (pathname-name pathname))
         (type (pathname-type pathname))
         (file (cond ((and name (stringp type)) (format nil "~a.~a" name type))
                     ((and name (plusp (length name))) name))))
    (format nil "/~{~a~^/~}" (append comps (and file (list file))))))

(defun %fs-key (fs pathname)
  (%posix-of (pathlib:fs-normpath fs (pathlib:fs-absolute fs pathname))))

(defun %fs-entry (fs pathname)
  "→ (values plist key) for PATHNAME, plist NIL when it does not exist."
  (%fs-load fs)
  (let ((key (%fs-key fs pathname)))
    (values (gethash key (%fs-entries fs)) key)))

(defun %entry-dir-p (e) (and e (member (getf e :type) '(:tree :commit)) t))
(defun %entry-file-p (e) (and e (eq (getf e :type) :blob)))
(defun %entry-link-p (e) (and (%entry-file-p e) (eql (getf e :mode) #o120000)))

(defun %fs-missing (fs pathname)
  (error 'pathlib:path-not-found :filesystem fs :path pathname
                                 :message "no such entry in the revision"))

(defun %fs-file-entry (fs pathname)
  (let ((e (%fs-entry fs pathname)))
    (unless (%entry-file-p e) (%fs-missing fs pathname))
    e))

(defun %fs-read-only (fs operation pathname)
  (error 'pathlib:unsupported-operation
         :filesystem fs :path pathname
         :message (format nil "~a is read-only: ~(~a~) is not supported" fs operation)))

;;; ------------------------------------------------------------------
;;; Paths (POSIX, rooted at /): the pure path GFs of a private
;;; memory filesystem, which has the same POSIX semantics.
;;; ------------------------------------------------------------------

(defmethod pathlib:fs-parse ((fs revision-filesystem) designator &key directory)
  (pathlib:fs-parse (%fs-paths fs)
                    (if (pathlib:path-p designator) (pathlib:path-pathname designator) designator)
                    :directory directory))

(defmethod pathlib:fs-join ((fs revision-filesystem) base relative)
  (pathlib:fs-join (%fs-paths fs) base relative))

(defmethod pathlib:fs-parent ((fs revision-filesystem) pathname)
  (pathlib:fs-parent (%fs-paths fs) pathname))

(defmethod pathlib:fs-name ((fs revision-filesystem) pathname)
  (pathlib:fs-name (%fs-paths fs) pathname))

(defmethod pathlib:fs-absolute ((fs revision-filesystem) pathname &key (defaults "/"))
  (pathlib:fs-absolute (%fs-paths fs) pathname :defaults defaults))

(defmethod pathlib:fs-normpath ((fs revision-filesystem) pathname)
  (pathlib:fs-normpath (%fs-paths fs) pathname))

(defmethod pathlib:fs-expanduser ((fs revision-filesystem) pathname)
  (pathlib:fs-parse fs pathname))

(defmethod pathlib:fs-resolve ((fs revision-filesystem) pathname &key (strict t))
  (let ((abs (pathlib:fs-normpath fs (pathlib:fs-absolute fs pathname))))
    (if (or (not strict) (%fs-entry fs abs))
        abs
        (restart-case (%fs-missing fs abs)
          (use-value (value)
            :report "Use a supplied value instead"
            value)))))

(defmethod pathlib:fs-same-p ((fs revision-filesystem) a b)
  (string= (%fs-key fs a) (%fs-key fs b)))

;;; ------------------------------------------------------------------
;;; Queries
;;; ------------------------------------------------------------------

(defmethod pathlib:fs-exists-p ((fs revision-filesystem) pathname)
  (and (%fs-entry fs pathname) t))

(defmethod pathlib:fs-file-p ((fs revision-filesystem) pathname)
  (%entry-file-p (%fs-entry fs pathname)))

(defmethod pathlib:fs-directory-p ((fs revision-filesystem) pathname)
  (%entry-dir-p (%fs-entry fs pathname)))

(defmethod pathlib:fs-symlink-p ((fs revision-filesystem) pathname)
  (%entry-link-p (%fs-entry fs pathname)))

(defmethod pathlib:fs-readable-p ((fs revision-filesystem) pathname)
  (pathlib:fs-exists-p fs pathname))

(defmethod pathlib:fs-writable-p ((fs revision-filesystem) pathname)
  (declare (ignore pathname))
  nil)

(defmethod pathlib:fs-executable-p ((fs revision-filesystem) pathname)
  (let ((e (%fs-entry fs pathname)))
    (and (%entry-file-p e) (eql (getf e :mode) #o100755))))

(defmethod pathlib:fs-file-size ((fs revision-filesystem) pathname)
  (or (getf (%fs-file-entry fs pathname) :size) 0))

(defmethod pathlib:fs-last-modified ((fs revision-filesystem) pathname)
  (unless (%fs-entry fs pathname) (%fs-missing fs pathname))
  (%fs-mtime fs))

(defmethod pathlib:fs-iterdir ((fs revision-filesystem) pathname)
  (multiple-value-bind (e key) (%fs-entry fs pathname)
    (unless (%entry-dir-p e) (%fs-missing fs pathname))
    (mapcar (lambda (name)
              (pathlib:fs-parse fs (if (string= key "/")
                                       (concatenate 'string "/" name)
                                       (concatenate 'string key "/" name))))
            (gethash key (%fs-children fs)))))

(defmethod pathlib:fs-read-bytes ((fs revision-filesystem) pathname)
  (let ((e (%fs-entry fs pathname)))
    (if (%entry-file-p e)
        (multiple-value-bind (type octets)
            (%batch-object (revision-filesystem-store fs) "contents" (getf e :id))
          (declare (ignore type))
          (%octets octets))
        (restart-case (%fs-missing fs pathname)
          (use-value (value)
            :report "Use a supplied value instead"
            value)))))

(defmethod pathlib:fs-read-symlink ((fs revision-filesystem) pathname)
  (let ((e (%fs-entry fs pathname)))
    (unless (%entry-link-p e)
      (error 'pathlib:path-error :filesystem fs :path pathname :message "not a symlink"))
    (pathlib:fs-parse fs (%utf8-decode (pathlib:fs-read-bytes fs pathname)))))

(defmethod pathlib:fs-stat ((fs revision-filesystem) pathname &key (follow-symlinks t))
  (declare (ignore follow-symlinks))
  (let ((e (%fs-entry fs pathname)))
    (unless e (%fs-missing fs pathname))
    (make-instance 'pathlib:file-stat
                   :path (pathlib:fs-parse fs pathname)
                   :kind (cond ((%entry-dir-p e) :directory)
                               ((%entry-link-p e) :symlink)
                               (t :file))
                   :size (if (%entry-file-p e) (or (getf e :size) 0) 0)
                   :mtime (%fs-mtime fs)
                   :mode (if (eql (getf e :mode) #o100755) #o755 #o644)
                   :filesystem fs)))

;;; Glob / walk (as zip-filesystem)

(defun %fs-ignore (fs base ignore)
  "Matcher for IGNORE (NIL, an IGNORE-MATCHER or :AUTO) at BASE."
  (etypecase ignore
    (null nil)
    (pathlib:ignore-matcher ignore)
    ((eql :auto) (pathlib:merge-ignore (pathlib:load-ignore-files fs base)
                                       (pathlib:parse-ignore '(".git/"))))))

(defun %relative-key (prefix key)
  (subseq key (length prefix)))

(defun %wild-pattern (pattern)
  (cond ((string= pattern "*") (make-pathname :name :wild :type :wild))
        ((and (>= (length pattern) 2) (char= (char pattern 0) #\*) (char= (char pattern 1) #\.))
         (make-pathname :name :wild :type (subseq pattern 2)))
        ((find #\* pattern) (uiop:parse-unix-namestring pattern :wilden t))
        (t (uiop:parse-unix-namestring pattern))))

(defun %name-matches-p (name pattern)
  (or (string= name pattern)
      (pathname-match-p (uiop:parse-unix-namestring name) (%wild-pattern pattern))))

(defmethod pathlib:fs-glob ((fs revision-filesystem) pathname pattern &key recursive ignore)
  (multiple-value-bind (e base-key) (%fs-entry fs pathname)
    (declare (ignore e))
    (let ((prefix (if (string= base-key "/") "/" (concatenate 'string base-key "/")))
          (matcher (%fs-ignore fs pathname ignore))
          (out '()))
      (labels ((visit (key)
                 (dolist (name (gethash key (%fs-children fs)))
                   (let* ((child (if (string= key "/")
                                     (concatenate 'string "/" name)
                                     (concatenate 'string key "/" name)))
                          (rel (%relative-key prefix child))
                          (dir-p (%entry-dir-p (gethash child (%fs-entries fs)))))
                     (unless (and matcher (pathlib:ignore-path-p matcher rel :directory dir-p))
                       (when (and (or recursive (find #\/ pattern) (not (find #\/ rel)))
                                  (if (find #\/ pattern)
                                      (pathname-match-p (uiop:parse-unix-namestring rel)
                                                        (%wild-pattern pattern))
                                      (%name-matches-p name pattern)))
                         (push (pathlib:fs-parse fs child) out))
                       (when (and dir-p (or recursive (find #\/ pattern)))
                         (visit child)))))))
        (when (gethash base-key (%fs-children fs))
          (visit base-key)))
      (nreverse out))))

(defmethod pathlib:fs-walk ((fs revision-filesystem) pathname &key (top-down t) follow-symlinks ignore)
  (declare (ignore follow-symlinks))
  (let* ((root (pathlib:fs-absolute fs pathname))
         (matcher (%fs-ignore fs root ignore))
         (prefix (let ((k (%fs-key fs root))) (if (string= k "/") "/" (concatenate 'string k "/"))))
         (out '()))
    (flet ((ignored-p (p dir-p)
             (and matcher
                  (pathlib:ignore-path-p matcher (%relative-key prefix (%fs-key fs p))
                                         :directory dir-p))))
      (labels ((walk* (dir)
                 (let* ((entries (pathlib:fs-iterdir fs dir))
                        (files (remove-if-not (lambda (p) (pathlib:fs-file-p fs p)) entries))
                        (dirs (remove-if-not (lambda (p) (pathlib:fs-directory-p fs p)) entries)))
                   (setf files (remove-if (lambda (f) (ignored-p f nil)) files)
                         dirs (remove-if (lambda (d) (ignored-p d t)) dirs))
                   (when top-down (push (list dir files dirs) out))
                   (dolist (d dirs) (walk* d))
                   (unless top-down (push (list dir files dirs) out)))))
        (walk* root)
        (nreverse out)))))

;;; ------------------------------------------------------------------
;;; Read-only: every mutating GF, fs-make-temp and fs-as-uri
;;; ------------------------------------------------------------------

(defmethod pathlib:fs-mkdir ((fs revision-filesystem) pathname &key parents exist-ok)
  (declare (ignore parents exist-ok))
  (%fs-read-only fs 'fs-mkdir pathname))

(defmethod pathlib:fs-rmdir ((fs revision-filesystem) pathname)
  (%fs-read-only fs 'fs-rmdir pathname))

(defmethod pathlib:fs-unlink ((fs revision-filesystem) pathname &key missing-ok)
  (declare (ignore missing-ok))
  (%fs-read-only fs 'fs-unlink pathname))

(defmethod pathlib:fs-touch ((fs revision-filesystem) pathname &key exist-ok)
  (declare (ignore exist-ok))
  (%fs-read-only fs 'fs-touch pathname))

(defmethod pathlib:fs-rename ((fs revision-filesystem) source target &key replace)
  (declare (ignore target replace))
  (%fs-read-only fs 'fs-rename source))

(defmethod pathlib:fs-copy ((fs revision-filesystem) source target &key replace)
  (declare (ignore target replace))
  (%fs-read-only fs 'fs-copy source))

(defmethod pathlib:fs-create-symlink ((fs revision-filesystem) link target)
  (declare (ignore target))
  (%fs-read-only fs 'fs-create-symlink link))

(defmethod pathlib:fs-write-bytes ((fs revision-filesystem) pathname bytes
                                   &key append if-exists if-does-not-exist)
  (declare (ignore bytes append if-exists if-does-not-exist))
  (%fs-read-only fs 'fs-write-bytes pathname))

(defmethod pathlib:fs-write-atomic ((fs revision-filesystem) pathname bytes &key expected-hash)
  (declare (ignore bytes expected-hash))
  (%fs-read-only fs 'fs-write-atomic pathname))

(defmethod pathlib:fs-make-temp ((fs revision-filesystem) &key directory prefix suffix)
  (declare (ignore prefix suffix))
  (%fs-read-only fs 'fs-make-temp directory))

(defmethod pathlib:fs-as-uri ((fs revision-filesystem) pathname)
  (%fs-read-only fs 'fs-as-uri pathname))
