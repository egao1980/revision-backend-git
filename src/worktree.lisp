(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; V1 — linked worktrees and replay.
;;;
;;; A linked worktree is another checkout of the same repository: refs and
;;; objects are shared, HEAD and the index are its own. ADD-WORKTREE returns
;;; a GIT-STORE opened on it (same :ENV and :PERMIT-FORCE), so
;;; (MAKE-GIT-WORKING-COPY that-store) is its target.
;;;
;;; REPLAY-REVISIONS is `git replay`, which needs git >= *REPLAY-GIT-VERSION*
;;; while *MINIMUM-GIT-VERSION* is lower (the ci-base container has 2.43,
;;; cl-repository#101): it is the one capability-gated operation. Older git →
;;; VC-TOOL-VERSION-ERROR (:REQUIRED *REPLAY-GIT-VERSION*) before any git
;;; command runs; GIT-REPLAY-P says in advance.
;;; ------------------------------------------------------------------

(defparameter *replay-git-version* "2.44.0"
  "First git release with `git replay`. REPLAY-REVISIONS needs at least this.")

(define-condition replay-conflict (rev:revision-error)
  ((revision :initarg :revision :reader replay-conflict-revision :initform nil)
   (onto :initarg :onto :reader replay-conflict-onto :initform nil))
  (:report (lambda (c s)
             (format s "replaying ~a onto ~a conflicts"
                     (replay-conflict-revision c) (replay-conflict-onto c))))
  (:documentation "REPLAY-REVISIONS: a commit does not apply cleanly. REVISION
is the id of the first commit that conflicted (NIL when git does not say),
ONTO the resolved onto id. No ref was moved."))

(defgeneric git-store-common-dir (store)
  (:documentation "→ absolute directory pathname of the repository's common
git dir (shared refs and objects). Equal to GIT-STORE-GIT-DIR except for a
store opened on a linked worktree, whose git dir is
<common>/worktrees/<name>/."))

(defmethod git-store-common-dir ((store git-store))
  (%not-implemented 'git-store-common-dir))

(defun add-worktree (store path designator &key branch)
  "`git worktree add` → a GIT-STORE on the new linked worktree at PATH (a
directory that is absent or empty), checked out at the commit DESIGNATOR
names. BRANCH NIL: detached HEAD. BRANCH a short name: a new branch
refs/heads/BRANCH created at that commit; an existing BRANCH → REF-CONFLICT
(:NAME BRANCH :EXPECTED NIL :ACTUAL its id), nothing created. A bare STORE
works (the new worktree is the only checkout). Signals what RESOLVE-REVISION
signals; a PATH that is not empty → REVISION-ERROR."
  (declare (ignore store path designator branch))
  (%not-implemented 'add-worktree))

(defun list-worktrees (store)
  "→ fresh list of plists, one per worktree of STORE's repository, main
worktree first, from `git worktree list --porcelain -z`:
  :PATH      absolute directory pathname
  :HEAD      commit id, NIL when unborn (or for a bare main entry)
  :BRANCH    short branch name (\"main\"), NIL when detached
  :DETACHED  T / NIL       :BARE  T / NIL
  :LOCKED    NIL, T, or the reason string
  :PRUNABLE  NIL, T, or the reason string"
  (declare (ignore store))
  (%not-implemented 'list-worktrees))

(defun remove-worktree (store path &key force)
  "`git worktree remove [--force] PATH` → T. A worktree with local changes
without FORCE → GIT-COMMAND-ERROR, nothing removed. The main worktree, or a
PATH that is no worktree of STORE's repository → REVISION-ERROR."
  (declare (ignore store path force))
  (%not-implemented 'remove-worktree))

(defun parse-worktree-list (string)
  "Pure: `git worktree list --porcelain -z` output → list of plists with the
keys LIST-WORKTREES documents (:PATH as a native namestring, not a
pathname). Records are attribute lines each ended by NUL, a record ended by
an extra NUL. Unknown attributes are ignored. Malformed input (no
\"worktree\" line first, a HEAD that is not hex, a truncated record) →
REVISION-ERROR."
  (declare (ignore string))
  (%not-implemented 'parse-worktree-list))

(defun git-replay-p (store)
  "True when STORE's git (probed version) is at least *REPLAY-GIT-VERSION*."
  (declare (ignore store))
  (%not-implemented 'git-replay-p))

(defun replay-revisions (store revisions onto &key ref (expected nil expected-p))
  "`git replay` → fresh list of the new REVISIONs, parents first: each commit of
REVISIONS (a list of designators, a linear chain, parents first, as
REVISION-RANGE returns) re-applied onto ONTO, keeping message, author and
author date; committer date (DT:NOW). REF (a branch short name): moved to the
last new revision with CAS against EXPECTED (default: its current target);
failure → REF-CONFLICT with the V0 restarts. Nothing in the worktree or
index changes.
Older git → VC-TOOL-VERSION-ERROR (:TOOL \"git\" :REQUIRED
*REPLAY-GIT-VERSION*), before any git command. A merge commit in REVISIONS,
or REVISIONS that are not a chain → REVISION-ERROR. A conflict →
REPLAY-CONFLICT, no ref moved. REVISIONS NIL → NIL."
  (declare (ignore store revisions onto ref expected expected-p))
  (%not-implemented 'replay-revisions))
