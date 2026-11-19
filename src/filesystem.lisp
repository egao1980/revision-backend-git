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
   (revision :initarg :revision :reader revision-filesystem-revision))
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
  (declare (ignore store designator))
  (%not-implemented 'make-revision-filesystem))

(defun revision-path (store designator &optional (path "/"))
  "→ CL-STACK-PATHLIB:PATH for PATH (a POSIX string, default \"/\") on a fresh
REVISION-FILESYSTEM of DESIGNATOR in STORE."
  (declare (ignore store designator path))
  (%not-implemented 'revision-path))

(macrolet ((stub (name lambda-list)
             (let ((vars (remove-if (lambda (s) (member s '(&key &optional)))
                                    (mapcar (lambda (x) (if (consp x) (first x) x))
                                            (rest lambda-list)))))
               `(defmethod ,name ((fs revision-filesystem) ,@lambda-list)
                  (declare (ignore ,@vars))
                  (%not-implemented ',name)))))
  (stub pathlib:fs-parse (designator &key directory))
  (stub pathlib:fs-join (base relative))
  (stub pathlib:fs-parent (pathname))
  (stub pathlib:fs-name (pathname))
  (stub pathlib:fs-absolute (pathname &key defaults))
  (stub pathlib:fs-resolve (pathname &key strict))
  (stub pathlib:fs-normpath (pathname))
  (stub pathlib:fs-expanduser (pathname))
  (stub pathlib:fs-exists-p (pathname))
  (stub pathlib:fs-file-p (pathname))
  (stub pathlib:fs-directory-p (pathname))
  (stub pathlib:fs-symlink-p (pathname))
  (stub pathlib:fs-readable-p (pathname))
  (stub pathlib:fs-writable-p (pathname))
  (stub pathlib:fs-executable-p (pathname))
  (stub pathlib:fs-file-size (pathname))
  (stub pathlib:fs-last-modified (pathname))
  (stub pathlib:fs-iterdir (pathname))
  (stub pathlib:fs-glob (pathname pattern &key recursive ignore))
  (stub pathlib:fs-walk (pathname &key top-down follow-symlinks ignore))
  (stub pathlib:fs-mkdir (pathname &key parents exist-ok))
  (stub pathlib:fs-rmdir (pathname))
  (stub pathlib:fs-unlink (pathname &key missing-ok))
  (stub pathlib:fs-touch (pathname &key exist-ok))
  (stub pathlib:fs-rename (source target &key replace))
  (stub pathlib:fs-copy (source target &key replace))
  (stub pathlib:fs-read-symlink (pathname))
  (stub pathlib:fs-read-bytes (pathname))
  (stub pathlib:fs-write-bytes (pathname bytes &key append if-exists if-does-not-exist))
  (stub pathlib:fs-make-temp (&key directory prefix suffix))
  (stub pathlib:fs-same-p (a b))
  (stub pathlib:fs-as-uri (pathname))
  (stub pathlib:fs-stat (pathname &key follow-symlinks)))
