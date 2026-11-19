(in-package #:revision-backend-git/tests)

;;; Test support: throwaway repositories driven by the REAL git, independent
;;; of the code under test. Every repository lives in a fresh temp directory
;;; with its own HOME and an empty global config, so the user's ~/.gitconfig
;;; never applies; user.name / user.email / core.autocrlf are set per repo.
;;; Expected values are computed here by running git; the backend must agree.
;;;
;;; Git runs through UIOP:RUN-PROGRAM with stdout / stderr redirected to
;;; files, so captures are byte-exact (process-backend-uiop 0.1.0 decodes
;;; output as text; see the brief).

(defparameter *git* "git")

(defstruct (trepo (:constructor %make-trepo))
  root   ; absolute directory pathname of the temp root
  dir    ; the repository (worktree top-level, or the bare git dir)
  home   ; the test HOME
  env)   ; alist (name . value) for every git call, also given to the store

;;; ---- bytes and text --------------------------------------------------

(defun utf8 (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun from-utf8 (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(defun octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun lines (&rest strings)
  "STRINGS joined, each followed by a newline."
  (with-output-to-string (s)
    (dolist (x strings) (write-string x s) (write-char #\Newline s))))

(defun nul-join (&rest strings)
  "STRINGS each followed by a NUL character (git -z records)."
  (with-output-to-string (s)
    (dolist (x strings) (write-string x s) (write-char (code-char 0) s))))

(defun write-octets (pathname octets)
  (ensure-directories-exist pathname)
  (with-open-file (out pathname :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :supersede :if-does-not-exist :create)
    (write-sequence octets out))
  pathname)

(defun read-octets (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in)
      v)))

;;; ---- environment -------------------------------------------------------

(defun %inherited-environment ()
  "→ alist of the current process environment."
  (let ((entries #+sbcl (sb-ext:posix-environ)
                 #-sbcl (loop for name in '("PATH" "Path" "SYSTEMROOT" "SystemRoot" "TEMP"
                                            "TMP" "TMPDIR" "PATHEXT" "COMSPEC" "WINDIR")
                              for value = (uiop:getenv name)
                              when value collect (format nil "~a=~a" name value))))
    (loop for e in entries
          for p = (position #\= e :start 1)
          when p collect (cons (subseq e 0 p) (subseq e (1+ p))))))

(defun %environment (overrides)
  "Environment list (\"NAME=VALUE\" …): the inherited one with OVERRIDES
(alist) replacing entries of the same name (case-insensitive, for Windows)."
  (let ((base (remove-if (lambda (pair)
                           (assoc (car pair) overrides :test #'string-equal))
                         (%inherited-environment))))
    (loop for (k . v) in (append overrides base)
          collect (format nil "~a=~a" k v))))

(defparameter *fixed-test-env*
  '(("LC_ALL" . "C")
    ("GIT_CONFIG_NOSYSTEM" . "1")
    ("GIT_TERMINAL_PROMPT" . "0")
    ("GIT_PAGER" . "cat")
    ("GIT_EDITOR" . ":")))

;;; ---- running git -------------------------------------------------------

(defun %temp-file (root name)
  (merge-pathnames (format nil "~a-~36r" name (random (expt 36 8))) root))

(defun git* (repo args &key input env (ok '(0)) (output :string) (directory t))
  "Run real git ARGS in REPO (unless DIRECTORY is NIL) → (values stdout exit-code
stderr). INPUT: NIL, string or octets. ENV: extra alist on top of REPO's env.
OUTPUT :STRING (UTF-8) or :OCTETS. An exit code not in OK → ERROR."
  (let* ((root (trepo-root repo))
         (out (%temp-file root "out"))
         (err (%temp-file root "err"))
         (in (when input
               (write-octets (%temp-file root "in")
                             (if (stringp input) (utf8 input) input))))
         (argv (append (list *git*)
                       (when directory
                         (list "-C" (uiop:native-namestring (trepo-dir repo))))
                       args)))
    (unwind-protect
         (let* ((code (nth-value 2 (uiop:run-program
                                    argv
                                    :input in
                                    :output out
                                    :error-output err
                                    :if-output-exists :supersede
                                    :if-error-output-exists :supersede
                                    :ignore-error-status t
                                    :environment (%environment
                                                  (append env (trepo-env repo))))))
                (stdout (read-octets out))
                (stderr (from-utf8 (read-octets err))))
           (unless (member code ok)
             (error "test git ~{~a~^ ~} → ~a: ~a" args code stderr))
           (values (if (eq output :octets) stdout (from-utf8 stdout)) code stderr))
      (dolist (f (list out err in))
        (when f (ignore-errors (delete-file f)))))))

(defun git (repo &rest args)
  "Real git ARGS in REPO → stdout with one trailing newline removed."
  (string-right-trim '(#\Newline) (git* repo args)))

(defun git-lines (repo &rest args)
  "Real git ARGS in REPO → stdout lines (no empty ones)."
  (remove "" (uiop:split-string (git* repo args) :separator '(#\Newline))
          :test #'string=))

(defun rev (repo spec)
  "Full commit id of SPEC (real git)."
  (git repo "rev-parse" "--verify" (concatenate 'string spec "^{commit}")))

;;; ---- repositories ------------------------------------------------------

(defun %make-root ()
  (let ((root (merge-pathnames (format nil "rbg-test-~36r/" (random (expt 36 10)
                                                                      (make-random-state t)))
                               (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
    (ensure-directories-exist root)
    (truename root)))

(defun make-test-repo (&key bare (branch "main"))
  "Fresh repository in a fresh temp root → TREPO. HOME is <root>/home with an
empty .gitconfig; the repo has user.name \"Test User\", user.email
\"test@example.invalid\", core.autocrlf false, gpg signing off."
  (let* ((root (%make-root))
         (home (merge-pathnames "home/" root))
         (dir (merge-pathnames (if bare "repo.git/" "repo/") root))
         (global (merge-pathnames ".gitconfig" home)))
    (ensure-directories-exist home)
    (ensure-directories-exist dir)
    (write-octets global (octets))
    (let ((repo (%make-trepo
                 :root root :dir dir :home home
                 :env (append *fixed-test-env*
                              (list (cons "HOME" (uiop:native-namestring home))
                                    (cons "USERPROFILE" (uiop:native-namestring home))
                                    (cons "XDG_CONFIG_HOME"
                                          (uiop:native-namestring (merge-pathnames ".config/" home)))
                                    (cons "GIT_CONFIG_GLOBAL" (uiop:native-namestring global)))))))
      (git* repo (append (list "init" "-q" "-b" branch) (when bare (list "--bare"))))
      (dolist (kv '(("user.name" "Test User") ("user.email" "test@example.invalid")
                    ("core.autocrlf" "false") ("core.safecrlf" "false")
                    ("commit.gpgsign" "false") ("tag.gpgsign" "false")
                    ("gc.auto" "0")))
        (git* repo (list "config" (first kv) (second kv))))
      repo)))

(defun delete-test-repo (repo)
  (ignore-errors (uiop:delete-directory-tree (trepo-root repo) :validate t)))

(defmacro with-test-repo ((var &rest keys) &body body)
  `(let ((,var (make-test-repo ,@keys)))
     (unwind-protect (progn ,@body)
       (delete-test-repo ,var))))

(defun repo-file (repo relative)
  (merge-pathnames relative (trepo-dir repo)))

(defun put-file (repo relative content)
  "Write CONTENT (string → UTF-8 bytes exactly, no newline translation; or
octets) at RELATIVE in REPO's worktree."
  (write-octets (repo-file repo relative)
                (if (stringp content) (utf8 content) content)))

(defun remove-file (repo relative)
  (delete-file (repo-file repo relative)))

(defun date-env (at &optional author-at)
  (list (cons "GIT_COMMITTER_DATE" (format nil "@~d +0000" at))
        (cons "GIT_AUTHOR_DATE" (format nil "@~d +0000" (or author-at at)))))

(defun commit-all (repo message &key (at 1700000000) author-at)
  "git add -A; git commit with committer date AT and author date AUTHOR-AT
(default AT), unix seconds → the new HEAD id."
  (git* repo '("add" "-A"))
  (let ((msg (%temp-file (trepo-root repo) "msg")))
    (write-octets msg (utf8 message))
    (unwind-protect
         (git* repo (list "commit" "-q" "--allow-empty" "-F" (uiop:native-namestring msg))
               :env (date-env at author-at))
      (delete-file msg)))
  (rev repo "HEAD"))

(defun hash-blob (repo content)
  "Write CONTENT as a blob (real git) → its id."
  (string-right-trim '(#\Newline)
                     (git* repo '("hash-object" "-w" "--stdin")
                           :input (if (stringp content) (utf8 content) content))))

(defun index-entry (repo mode id path)
  "Add an index-only entry (works for symlinks, gitlinks and names the local
filesystem cannot hold)."
  (git* repo (list "update-index" "--add" "--cacheinfo"
                   (format nil "~a,~a,~a" mode id path))))

(defun commit-index (repo message &key (at 1700000000) author-at)
  "Commit the index as is (no add -A) → HEAD id."
  (let ((msg (%temp-file (trepo-root repo) "msg")))
    (write-octets msg (utf8 message))
    (unwind-protect
         (git* repo (list "commit" "-q" "--allow-empty" "-F" (uiop:native-namestring msg))
               :env (date-env at author-at))
      (delete-file msg)))
  (rev repo "HEAD"))

;;; ---- the store under test ---------------------------------------------

(defun store-env (repo)
  "The :ENV a test store gets: the isolation variables of REPO (HOME, global
config …), never the user's."
  (remove-if (lambda (pair) (assoc (car pair) *fixed-test-env* :test #'string=))
             (trepo-env repo)))

(defun open-store (repo &rest keys)
  (apply #'git:make-git-store (trepo-dir repo) :env (store-env repo) keys))

(defmacro with-store ((var repo &rest keys) &body body)
  `(let ((,var (open-store ,repo ,@keys)))
     (unwind-protect (progn ,@body)
       (ignore-errors (git:close-git-store ,var)))))

(defmacro with-repo-store ((repo-var store-var &rest repo-keys) setup &body body)
  "Fresh repo REPO-VAR, run SETUP (a form using REPO-VAR), then BODY with
STORE-VAR open on it."
  `(with-test-repo (,repo-var ,@repo-keys)
     ,setup
     (with-store (,store-var ,repo-var)
       ,@body)))

;;; ---- small helpers -------------------------------------------------------

(defun ids (revisions)
  (mapcar #'rev:revision-id revisions))

(defun set= (a b)
  (and (subsetp a b :test #'equal) (subsetp b a :test #'equal)))

(defun unix (instant)
  (dt:instant-seconds instant))

(defun instant (seconds)
  (dt:make-instant seconds))

(defun plist-matches-p (expected actual)
  "Every key of EXPECTED has an EQUALP value in ACTUAL."
  (loop for (k v) on expected by #'cddr
        always (equalp v (getf actual k '%missing))))

(defmacro capture-condition ((type) &body body)
  "Run BODY → the condition of TYPE it signalled, or NIL. Outside any OK."
  (let ((c (gensym "C")))
    `(handler-case (progn ,@body nil)
       (,type (,c) ,c))))

(defun restart-names-in (type thunk)
  "Call THUNK; on the first condition of TYPE record the names of the
restarts visible inside the handler, then unwind. → (values names condition)."
  (let ((names nil) (cond nil))
    (block run
      (handler-bind ((condition
                       (lambda (c)
                         (when (typep c type)
                           (setf cond c
                                 names (mapcar #'restart-name (compute-restarts c)))
                           (return-from run)))))
        (funcall thunk)))
    (values names cond)))

(defun tree-files (tree &optional prefix)
  "→ alist (\"a/b\" . file-node) of every file in a DIFF-PROTOCOL tree value."
  (loop for (name node) in (diff:tree-entries tree)
        for path = (if prefix (format nil "~a/~a" prefix name) name)
        nconc (if (diff:tree-file-p node)
                  (list (cons path node))
                  (tree-files node path))))

(defun tree-engine ()
  (diff:find-diff-engine :tree))

(defun state= (a b)
  (diff:state-equal (tree-engine) a b))
