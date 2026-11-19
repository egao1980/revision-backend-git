(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; The git program, its version, and how every git command runs.
;;;
;;; Milestone V0 ships the classes, readers and documented behaviour with
;;; STUB methods: every operation below signals a plain SIMPLE-ERROR (never a
;;; REVISION-ERROR, GIT-COMMAND-ERROR or VC-TOOL-VERSION-ERROR), so the test
;;; suite loads and fails without false passes. Implementers add slots and
;;; helpers freely; exported names, lambda lists and docstrings are the
;;; contract (with the brief, cl-stack docs/capabilities/revision-backend-git.md).
;;; ------------------------------------------------------------------

(defun %not-implemented (operation)
  (error "revision-backend-git: ~s is not implemented (V0 contract stub)"
         operation))

(defparameter *git-program* "git"
  "The git executable MAKE-GIT-STORE uses when :GIT is not given: a program
name looked up on PATH, or an absolute path (a string or pathname).")

(defparameter *minimum-git-version* "2.43.0"
  "Oldest git release the backend accepts, as a version string. Locked for V0
to the oldest git on the CI runners (the cl-repository ci-base container,
Ubuntu 24.04: 2.43.0); see the brief for the per-runner table. MAKE-GIT-STORE
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
  (declare (ignore string))
  (%not-implemented 'parse-git-version))

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
   (listener :initform nil))
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
  "→ GIT-STORE over the repository at PATH (a string or pathname naming the
top-level directory of a non-bare worktree, or a bare repository / git dir).

First probes `GIT --version` (once per program per image is enough; the
check against *MINIMUM-GIT-VERSION* runs on every call). A version older than
*MINIMUM-GIT-VERSION* signals VC-TOOL-VERSION-ERROR (:TOOL \"git\", :FOUND the
version string, e.g. \"2.42.1\", :REQUIRED *MINIMUM-GIT-VERSION*); a program
that cannot be started signals VC-TOOL-VERSION-ERROR with :FOUND NIL. No other
git command runs before that check.

PATH that is not a repository top-level (or does not exist) signals
GIT-COMMAND-ERROR (from `rev-parse`). GIT-STORE-PATH is then the absolute
directory pathname of the worktree top-level (NIL for a bare repository) and
GIT-STORE-GIT-DIR the absolute directory pathname of the git dir.

ENV: alist of (NAME . VALUE) strings added to the environment of every git
command this store runs (HOME, GIT_CONFIG_GLOBAL, GIT_AUTHOR_NAME …); the
fixed variables (LC_ALL=C, GIT_TERMINAL_PROMPT=0, GIT_CONFIG_NOSYSTEM=1,
GIT_OPTIONAL_LOCKS=0, GIT_PAGER=cat, GIT_EDITOR=:) win over ENV.
PERMIT-FORCE: NIL, T, or a predicate of the ref name — REVISION-PROTOCOL:
PERMIT-FORCE-P, the gate of the FORCE restart."
  (declare (ignore path git env permit-force))
  (%not-implemented 'make-git-store))

(defun git-store-env (store)
  "→ fresh copy of the :ENV alist STORE was made with."
  (copy-alist (%git-store-env store)))

(defun git-version (store)
  "→ the version of STORE's git program as a dotted string of the numeric
components `git --version` reported (\"2.54.0\"; \"2.47.1\" for
\"2.47.1.windows.2\")."
  (declare (ignore store))
  (%not-implemented 'git-version))

(defun run-git (store args &key input (output :string) (ok-exit-codes '(0)))
  "Run STORE's git with the fixed environment, the fixed options, then ARGS (a
list of strings), in STORE's worktree top-level (its git dir when bare).
→ (values stdout exit-code stderr).

INPUT: NIL, a string (sent as UTF-8) or an (unsigned-byte 8) vector, written
to standard input. OUTPUT :STRING decodes stdout as UTF-8; :OCTETS returns the
exact bytes as an (unsigned-byte 8) vector. STDERR is always a string.
An exit code not in OK-EXIT-CODES signals GIT-COMMAND-ERROR (:COMMAND ARGS,
:EXIT-CODE, :STDERR). Runs through PROCESS-PROTOCOL (*PROCESS-BACKEND*)."
  (declare (ignore store args input output ok-exit-codes))
  (%not-implemented 'run-git))

(defun git-store-batch-active-p (store)
  "True while STORE's long-lived `cat-file --batch-command` process is
running: after the first object read, until CLOSE-GIT-STORE."
  (declare (ignore store))
  (%not-implemented 'git-store-batch-active-p))

(defun close-git-store (store)
  "Stop STORE's batch process (if any) → T. Idempotent. The store stays
usable: a later object read starts a new batch process."
  (declare (ignore store))
  (%not-implemented 'close-git-store))

(defmacro with-git-store ((var path &rest keys) &body body)
  "Bind VAR to (MAKE-GIT-STORE PATH . KEYS) around BODY; CLOSE-GIT-STORE on any
exit."
  `(let ((,var (make-git-store ,path ,@keys)))
     (unwind-protect (progn ,@body)
       (close-git-store ,var))))
