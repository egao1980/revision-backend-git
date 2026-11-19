(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; The git program, its version, and how every git command runs.
;;;
;;; Every git process runs through PROCESS-PROTOCOL (*PROCESS-BACKEND*):
;;; %RUN-GIT-PROGRAM builds the argv (program, fixed options, arguments) and
;;; the environment (inherited + store :ENV + per-call extras + the fixed
;;; variables, which win), captures stdout / stderr as octets and translates
;;; an unexpected exit status into GIT-COMMAND-ERROR. Object reads go through
;;; one long-lived `cat-file --batch-command` process per store (the BATCH
;;; section below). Exported names, lambda lists and docstrings are the
;;; contract (with the brief, cl-stack docs/capabilities/revision-backend-git.md).
;;; ------------------------------------------------------------------

(defparameter *git-program* "git"
  "The git executable MAKE-GIT-STORE uses when :GIT is not given: a program
name looked up on PATH, or an absolute path (a string or pathname).")

(defparameter *minimum-git-version* "2.43.0"
  "Oldest git release the backend accepts, as a version string. Locked for V0
to the oldest git on the CI runners, probed 2026-11-19: ci-base container
(Ubuntu 24.04, where the ubuntu test job runs) 2.43.0; ubuntu-latest VM 2.56.1;
macos-latest 2.56.0; windows-latest 2.56.0.windows.4. MAKE-GIT-STORE
compares the probed version with the value of this variable at call time, so
binding it higher makes any git look too old (tests do this).")

;;; The fixed environment and options of every git command. Variables are
;;; added to the inherited environment (they override inherited values and
;;; any :ENV entry of the same name); options come right after the program
;;; name, before the caller's arguments.

(defparameter +fixed-environment+
  '(("LC_ALL" . "C")
    ("GIT_TERMINAL_PROMPT" . "0")
    ("GIT_CONFIG_NOSYSTEM" . "1")
    ("GIT_OPTIONAL_LOCKS" . "0")
    ("GIT_PAGER" . "cat")
    ("GIT_EDITOR" . ":"))
  "Environment variables set for every git command (name . value).")

(defparameter +fixed-options+
  '("-c" "core.quotepath=off"
    "-c" "color.ui=never"
    "-c" "core.autocrlf=false"
    "-c" "core.safecrlf=false"
    "-c" "diff.noprefix=false"
    "-c" "diff.mnemonicPrefix=false"
    "-c" "core.disambiguate=commit"
    "--no-pager")
  "Global options placed before the caller's arguments in every git command.
Commands that print diffs or paths also get -z / --no-color / --no-ext-diff /
--no-textconv where git accepts them; that is per command, not here.")

(defun parse-git-version (string)
  "→ list of non-negative integers (major minor patch …) parsed from STRING:
either the output of `git --version` (\"git version 2.54.0 (Apple Git-157)\",
\"git version 2.47.1.windows.2\") or a bare version (\"2.43.0\", \"2.43\").
Parsing stops at the first component that is not all digits, so vendor
suffixes are dropped: \"2.47.1.windows.2\" → (2 47 1). A string with no
leading numeric component → NIL. Missing trailing components are not padded
(\"2.43\" → (2 43)); comparisons treat them as 0."
  (check-type string string)
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) string))
         (prefix "git version ")
         (s (if (and (>= (length s) (length prefix))
                     (string-equal prefix s :end2 (length prefix)))
                (subseq s (length prefix))
                s))
         (token (subseq s 0 (or (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                             s)
                                (length s)))))
    (loop for part in (%split-string token #\.)
          while (and (plusp (length part)) (every #'digit-char-p part))
          collect (parse-integer part))))

(defun %version< (a b)
  "True when version list A is older than B (missing components read as 0)."
  (loop for i below (max (length a) (length b))
        for x = (or (nth i a) 0)
        for y = (or (nth i b) 0)
        do (cond ((< x y) (return t))
                 ((> x y) (return nil)))
        finally (return nil)))

(defun %version-string (version)
  (format nil "~{~d~^.~}" version))

;;; ------------------------------------------------------------------
;;; Small text helpers (no git)
;;; ------------------------------------------------------------------

(defun %split-string (string char)
  "STRING split at every CHAR → list of fresh strings (empty ones kept)."
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun %lines (string)
  "STRING split at newlines, without the empty string after a final newline."
  (let ((parts (%split-string string #\Newline)))
    (if (and parts (string= "" (car (last parts))))
        (butlast parts)
        parts)))

(defun %chomp (string)
  "STRING without one trailing newline."
  (let ((n (length string)))
    (if (and (plusp n) (char= #\Newline (char string (1- n))))
        (subseq string 0 (1- n))
        string)))

(defun %octets (sequence)
  "SEQUENCE (a vector of bytes) as a fresh simple (unsigned-byte 8) vector."
  (let ((v (make-array (length sequence) :element-type '(unsigned-byte 8))))
    (replace v sequence)
    v))

(defun %utf8-encode (string)
  "STRING → UTF-8 octets."
  (%octets (encoding-protocol:encode string :encoding :utf-8)))

(defun %utf8-decode-strict (octets)
  "OCTETS decoded as UTF-8 → string, or NIL when they are not valid UTF-8."
  (if (zerop (length octets))
      (make-string 0)
      (handler-case (encoding-protocol:decode (%octets octets) :encoding :utf-8)
        (encoding-protocol:encoding-error () nil))))

(defun %utf8-decode (octets)
  "OCTETS decoded as UTF-8; a line that is not valid UTF-8 is read byte by
byte as Latin-1 instead (git prints raw bytes of non-UTF-8 files in patches
and messages). Never signals."
  (or (%utf8-decode-strict octets)
      (with-output-to-string (out)
        (let ((start 0) (n (length octets)))
          (loop while (< start n)
                do (let* ((lf (position 10 octets :start start))
                          (end (if lf (1+ lf) n))
                          (line (subseq octets start end)))
                     (let ((s (%utf8-decode-strict line)))
                       (if s
                           (write-string s out)
                           (loop for b across line do (write-char (code-char b) out))))
                     (setf start end)))))))

;;; ------------------------------------------------------------------
;;; The store
;;; ------------------------------------------------------------------

(defclass git-store (rev:revision-store)
  ((path :initarg :path :reader git-store-path)
   (git-dir :initarg :git-dir :reader git-store-git-dir)
   (bare-p :initarg :bare-p :reader git-store-bare-p)
   (env :initarg :env :initform '() :reader %git-store-env)
   (git :initarg :git :reader %git-store-program)
   (version :initarg :version :reader %git-store-version)
   (permit-force :initarg :permit-force :initform nil
                 :reader %git-store-permit-force)
   (listener :initform nil :accessor %git-store-listener)
   (listener-lock :initform (bt2:make-lock :name "git-store listener")
                  :reader %git-store-listener-lock)
   (ref-lock :initform (bt2:make-lock :name "git-store refs")
             :reader %git-store-ref-lock
             :documentation "Serializes this store's read-compare-update-ref
sections; git's own ref lock covers other processes.")
   (batch :initform nil :accessor %git-store-batch
          :documentation "The live %BATCH session, or NIL.")
   (batch-lock :initform (bt2:make-lock :name "git-store batch")
               :reader %git-store-batch-lock)
   (cache :initform (make-hash-table :test 'equal) :reader %git-store-cache
          :documentation "Commit id → REVISION (revisions are immutable).")
   (cache-lock :initform (bt2:make-lock :name "git-store cache")
               :reader %git-store-cache-lock)
   (object-format :initform nil :accessor %git-store-object-format)
   (empty-tree :initform nil :accessor %git-store-empty-tree))
  (:documentation "A REVISION-STORE over one git repository, driven through the
git CLI (no libgit2 / gitoxide).

Revisions are commits: REVISION-ID is the full lowercase hex object name (40
characters for SHA-1 repositories, 64 for SHA-256); REVISION-PARENT-IDS the
commit's parents in order; REVISION-RECORDED-AT the committer date and
REVISION-VALID-AT the author date (DATETIME-PROTOCOL instants, whole seconds);
REVISION-MESSAGE the commit message without its final newline;
REVISION-PROVENANCE the plist (:AUTHOR-NAME s :AUTHOR-EMAIL s :COMMITTER-NAME s
:COMMITTER-EMAIL s :TREE tree-id); REVISION-CONTENT the tree id (a string).

Refs: movable refs are branches (refs/heads/NAME), tags are refs/tags/NAME;
protocol ref names are the short names (\"main\", \"feature/x\", \"v1.0\").
CONTENT-PRIMARY is :STATE; states are DIFF-PROTOCOL tree values; the empty
state is (DIFF-PROTOCOL:MAKE-TREE).

The revision graph that HEADS, REVISION-CHILDREN and walks without an
explicit FROM consider is every commit reachable from refs/heads/*,
refs/tags/* and HEAD. Any commit object can still be resolved by id.

Object reads go through one long-lived `git cat-file --batch-command`
process per store, started on first use and stopped by CLOSE-GIT-STORE.
A store may be used from several threads; operations that read or update
refs are linearizable with respect to each other and to other git processes
(git's own ref locking via update-ref)."))

(defun git-store-p (x)
  "T when X is a GIT-STORE."
  (typep x 'git-store))

(defun make-git-store (path &key (git *git-program*) env permit-force)
  "→ GIT-STORE over the repository at PATH (a string or pathname naming a
directory inside a non-bare worktree — usually its top-level — or a bare
repository / git dir).

First probes `GIT --version` (once per program per image is enough; the
check against *MINIMUM-GIT-VERSION* runs on every call). A version older than
*MINIMUM-GIT-VERSION* signals VC-TOOL-VERSION-ERROR (:TOOL \"git\", :FOUND the
version string, e.g. \"2.42.1\", :REQUIRED *MINIMUM-GIT-VERSION*); a program
that cannot be started signals VC-TOOL-VERSION-ERROR with :FOUND NIL. No other
git command runs before that check.

PATH outside any repository (or missing) signals GIT-COMMAND-ERROR (from
`rev-parse`). GIT-STORE-PATH is then the absolute
directory pathname of the worktree top-level (NIL for a bare repository) and
GIT-STORE-GIT-DIR the absolute directory pathname of the git dir.

ENV: alist of (NAME . VALUE) strings added to the environment of every git
command this store runs (HOME, GIT_CONFIG_GLOBAL, GIT_AUTHOR_NAME …); the
fixed variables (LC_ALL=C, GIT_TERMINAL_PROMPT=0, GIT_CONFIG_NOSYSTEM=1,
GIT_OPTIONAL_LOCKS=0, GIT_PAGER=cat, GIT_EDITOR=:) win over ENV.
PERMIT-FORCE: NIL, T, or a predicate of the ref name — REVISION-PROTOCOL:
PERMIT-FORCE-P, the gate of the FORCE restart."
  (check-type path (or string pathname))
  (check-type git (or string pathname))
  (check-type permit-force (or boolean function))
  (let* ((program (if (pathnamep git) (uiop:native-namestring git) git))
         (env (mapcar (lambda (e) (cons (string (car e)) (string (cdr e)))) env))
         (version (%probe-version program))
         (required *minimum-git-version*))
    (cond ((null version)
           (error 'vc-tool-version-error :tool "git" :found nil :required required))
          ((%version< version (parse-git-version required))
           (error 'vc-tool-version-error :tool "git" :found (%version-string version)
                                         :required required)))
    (let* ((native (uiop:native-namestring
                    (uiop:ensure-directory-pathname (uiop:ensure-absolute-pathname
                                                     (if (stringp path)
                                                         (uiop:parse-native-namestring path)
                                                         path)
                                                     (uiop:getcwd)))))
           (lines (%lines (%run-git-program
                           program env nil
                           (list "-C" native "rev-parse" "--absolute-git-dir"
                                 "--is-bare-repository"))))
           (git-dir (uiop:ensure-directory-pathname
                     (uiop:parse-native-namestring (first lines))))
           (bare (equal "true" (second lines)))
           (top (unless bare
                  (multiple-value-bind (out code)
                      (%run-git-program program env nil
                                        (list "-C" native "rev-parse" "--show-toplevel")
                                        :ok '(0 128))
                    (when (zerop code)
                      (uiop:ensure-directory-pathname
                       (uiop:parse-native-namestring (%chomp out))))))))
      (make-instance 'git-store :path top :git-dir git-dir :bare-p (or bare (null top))
                                :env env :git program :version version
                                :permit-force permit-force))))

;;; ------------------------------------------------------------------
;;; Running git
;;; ------------------------------------------------------------------

(defvar *version-cache* (make-hash-table :test 'equal)
  "Program string → probed version list (successful probes only).")

(defvar *version-cache-lock* (bt2:make-lock :name "git version cache"))

(defun %environment (env extra)
  "The :ENV for PROCESS-PROTOCOL:RUN: ENV (the store's), then EXTRA (per call),
then the fixed variables; later entries win (MERGE-ENVIRONMENT)."
  (append env extra +fixed-environment+))

(defun %probe-version (program)
  "→ version list of PROGRAM (`PROGRAM --version`), or NIL when it cannot be
started or prints no version. Cached per program."
  (or (bt2:with-lock-held (*version-cache-lock*)
        (gethash program *version-cache*))
      (let ((version
              (handler-case
                  (multiple-value-bind (code out)
                      (proc:run (list program "--version")
                                :output :octets :error-output :octets
                                :env +fixed-environment+)
                    (and (eql code 0) (parse-git-version (%utf8-decode out))))
                (proc:process-error () nil))))
        (when version
          (bt2:with-lock-held (*version-cache-lock*)
            (setf (gethash program *version-cache*) version)))
        version)))

(defun %run-git-program (program env directory args
                         &key input (output :string) (ok '(0)) extra-env)
  "Run PROGRAM with the fixed options and ARGS in DIRECTORY (NIL: the current
one) → (values stdout exit-code stderr). OUTPUT :STRING or :OCTETS. An exit
code not in OK, or a program that cannot run, → GIT-COMMAND-ERROR."
  (let ((argv (list* program (append +fixed-options+ args))))
    (multiple-value-bind (code out err)
        (handler-case
            (apply #'proc:run argv
                   :input input :output :octets :error-output :octets
                   :env (%environment env extra-env)
                   (when directory (list :directory directory)))
          (proc:process-error (c)
            (error 'git-command-error :command (copy-list args) :exit-code nil
                                      :stderr (princ-to-string c))))
      (let ((err (%utf8-decode err)))
        (unless (member code ok)
          (error 'git-command-error :command (copy-list args) :exit-code code
                                    :stderr err))
        (values (if (eq output :octets) (%octets out) (%utf8-decode out))
                code
                err)))))

(defun %store-directory (store)
  (or (git-store-path store) (git-store-git-dir store)))

(defun %git (store args &key input (output :string) (ok '(0)) extra-env)
  "RUN-GIT with per-call EXTRA-ENV (an alist added after the store's :ENV)."
  (%run-git-program (%git-store-program store) (%git-store-env store)
                    (%store-directory store) args
                    :input input :output output :ok ok :extra-env extra-env))

(defun %git-line (store args &rest keys)
  "First line of git's output (NIL when empty), → (values line exit-code)."
  (multiple-value-bind (out code) (apply #'%git store args keys)
    (values (first (%lines out)) code)))

(defun git-store-env (store)
  "→ fresh copy of the :ENV alist STORE was made with."
  (copy-alist (%git-store-env store)))

(defun git-version (store)
  "→ the version of STORE's git program as a dotted string of the numeric
components `git --version` reported (\"2.54.0\"; \"2.47.1\" for
\"2.47.1.windows.2\")."
  (%version-string (%git-store-version store)))

(defun run-git (store args &key input (output :string) (ok-exit-codes '(0)))
  "Run STORE's git with the fixed environment, the fixed options, then ARGS (a
list of strings), in STORE's worktree top-level (its git dir when bare).
→ (values stdout exit-code stderr).

INPUT: NIL, a string (sent as UTF-8) or an (unsigned-byte 8) vector, written
to standard input. OUTPUT :STRING decodes stdout as UTF-8; :OCTETS returns the
exact bytes as an (unsigned-byte 8) vector. STDERR is always a string.
An exit code not in OK-EXIT-CODES signals GIT-COMMAND-ERROR (:COMMAND ARGS,
:EXIT-CODE, :STDERR). Runs through PROCESS-PROTOCOL (*PROCESS-BACKEND*)."
  (check-type output (member :string :octets))
  (check-type input (or null string (vector (unsigned-byte 8))))
  (%git store (mapcar #'string args) :input input :output output :ok ok-exit-codes))

;;; ------------------------------------------------------------------
;;; The batch process: `cat-file --batch-command`, octet pipes, one per
;;; store, guarded by BATCH-LOCK. A session also memoizes the objects it
;;; read (id → (type . octets)); the memo goes away with the process.
;;; ------------------------------------------------------------------

(defstruct (%batch (:constructor %make-batch (handle stdin stdout err-file)))
  handle stdin stdout err-file
  (objects (make-hash-table :test 'equal)))

(defun %batch-launch (store)
  (let ((err (uiop:with-temporary-file (:pathname p :prefix "rbg-batch" :keep t) p)))
    (let ((handle (handler-case
                      (proc:launch (list* (%git-store-program store)
                                          (append +fixed-options+
                                                  '("cat-file" "--batch-command")))
                                   :directory (%store-directory store)
                                   :env (%environment (%git-store-env store) nil)
                                   :element-type :octets
                                   :error-output err)
                    (proc:process-error (c)
                      (uiop:delete-file-if-exists err)
                      (error 'git-command-error :command '("cat-file" "--batch-command")
                                                :exit-code nil :stderr (princ-to-string c))))))
      (%make-batch handle (proc:stdin handle) (proc:stdout handle) err))))

(defun %close-pipe (stream)
  "Close STREAM, a pipe of a batch process that may have died already."
  (handler-case (close stream)
    (stream-error () nil)))

(defun %batch-stop (batch)
  (%close-pipe (%batch-stdin batch))
  (let ((handle (%batch-handle batch)))
    (handler-case (proc:wait handle :timeout 5)
      (proc:process-timeout-error ()
        (proc:kill handle :force t)
        (proc:wait handle))))
  (%close-pipe (%batch-stdout batch))
  (uiop:delete-file-if-exists (%batch-err-file batch)))

(defun %read-header (stream)
  "One LF-terminated line of octets from STREAM, as a string (no LF)."
  (let ((bytes (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream)
          until (= b 10)
          do (vector-push-extend b bytes))
    (map 'string #'code-char bytes)))

(defun %batch-request (batch command id)
  "→ (values type octets) for COMMAND \"contents\", (values type size) for
\"info\", or NIL when ID names no object. Signals STREAM-ERROR / END-OF-FILE
when the process died."
  (let ((in (%batch-stdin batch))
        (out (%batch-stdout batch)))
    (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                         (format nil "~a ~a~c" command id #\Newline))
                    in)
    (finish-output in)
    (let* ((header (%read-header out))
           (fields (%split-string header #\Space)))
      (if (/= 3 (length fields))
          nil                           ; "<id> missing" / "<id> ambiguous"
          (let ((type (second fields))
                (size (parse-integer (third fields))))
            (if (string= command "info")
                (values type size)
                (let ((data (make-array size :element-type '(unsigned-byte 8))))
                  (let ((n (read-sequence data out)))
                    (when (< n size) (error 'end-of-file :stream out)))
                  (read-byte out)       ; the LF after the contents
                  (values type data))))))))

(defun %batch-object (store command id)
  "Object ID through STORE's batch process (started on demand, restarted once
when it died). COMMAND \"contents\" → (values type octets); \"info\" →
(values type size); NIL when ID names no object."
  (bt2:with-lock-held ((%git-store-batch-lock store))
    (let ((memo (let ((b (%git-store-batch store)))
                  (and b (gethash id (%batch-objects b))))))
      (if (and memo (or (string= command "info") (cdr memo)))
          (values (car memo) (if (string= command "info") (length (cdr memo)) (cdr memo)))
          (loop for attempt from 1
                do (let ((batch (or (%git-store-batch store)
                                    (setf (%git-store-batch store) (%batch-launch store)))))
                     (handler-case
                         (multiple-value-bind (type data) (%batch-request batch command id)
                           (when (and type (string= command "contents"))
                             (setf (gethash id (%batch-objects batch)) (cons type data)))
                           (return (values type data)))
                       ((or stream-error end-of-file) (c)
                         (setf (%git-store-batch store) nil)
                         (%batch-stop batch)
                         (when (>= attempt 2)
                           (error 'git-command-error
                                  :command '("cat-file" "--batch-command")
                                  :exit-code nil :stderr (princ-to-string c)))))))))))

(defun git-store-batch-active-p (store)
  "True while STORE's long-lived `cat-file --batch-command` process is
running: after the first object read, until CLOSE-GIT-STORE."
  (let ((batch (%git-store-batch store)))
    (and batch (proc:alive-p (%batch-handle batch)) t)))

(defun close-git-store (store)
  "Stop STORE's batch process (if any) → T. Idempotent. The store stays
usable: a later object read starts a new batch process."
  (bt2:with-lock-held ((%git-store-batch-lock store))
    (let ((batch (%git-store-batch store)))
      (setf (%git-store-batch store) nil)
      (when batch (%batch-stop batch))))
  t)

(defmacro with-git-store ((var path &rest keys) &body body)
  "Bind VAR to (MAKE-GIT-STORE PATH . KEYS) around BODY; CLOSE-GIT-STORE on any
exit."
  `(let ((,var (make-git-store ,path ,@keys)))
     (unwind-protect (progn ,@body)
       (close-git-store ,var))))
