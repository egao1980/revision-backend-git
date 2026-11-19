(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; GIT-WORKING-COPY — the store's worktree as a REVISION-PROTOCOL target.
;;;
;;; TARGET-STATE is the tree `git add -A` would record: tracked files as they
;;; are on disk plus untracked files that are not ignored (.gitignore,
;;; .git/info/exclude, core.excludesFile), read the same way as
;;; REVISION-STATE. The index does not count: staging a change neither causes
;;; nor clears drift. Computed through a temporary index (GIT_INDEX_FILE = a
;;; copy of the real index, `add -A`, `write-tree`), never the real one.
;;;
;;; The stamp. STAMP-REF NIL (default): the stamp is HEAD. TARGET-REVISIONS
;;; → (HEAD's commit id), NIL on an unborn branch; TARGET-STAMP moves HEAD
;;; like `reset --soft` (`update-ref HEAD`; the current branch moves, or a
;;; detached HEAD), leaving files and index alone. STAMP-REF a full ref name
;;; ("refs/checkpoints/agent"): the stamp is that ref; HEAD, branches and
;;; the index are never touched; an absent ref is an unstamped target (NIL).
;;; TARGET-STAMP with more than one revision → REVISION-ERROR (one stamp);
;;; NIL deletes STAMP-REF, and with the HEAD stamp is a REVISION-ERROR.
;;;
;;; TARGET-DRIFT → NIL when TARGET-STATE equals the stamped revision's state
;;; (the empty tree when unstamped), otherwise STATE-DIFF from the stamp to
;;; the working copy, level :CHANGES. `status --porcelain=v2 -z
;;; --untracked-files=all` with no entries is a sufficient (not necessary)
;;; condition for NIL when the stamp is HEAD.
;;;
;;; CAPTURE-REVISION → a new commit whose tree is TARGET-STATE, PARENTS
;;; (designators) defaulting to TARGET-REVISIONS, MESSAGE default
;;; "capture"; then stamps it, so TARGET-DRIFT is NIL afterwards. The working
;;; tree and the real index file stay byte-identical. (Not `stash create`:
;;; it skips untracked files and would leave drift behind.)
;;;
;;; V1, not V0: (SETF TARGET-STATE), PLAN-TRANSITION / EXECUTE-PLAN,
;;; projections, linked worktrees.
;;; ------------------------------------------------------------------

(defclass git-working-copy (rev:revision-target)
  ((store :initarg :store :reader working-copy-store)
   (stamp-ref :initarg :stamp-ref :initform nil :reader working-copy-stamp-ref))
  (:documentation "The worktree of a non-bare GIT-STORE as a revision target.
See the comment above the class in src/working-copy.lisp for the stamp,
state and drift rules."))

(defun git-working-copy-p (x)
  "T when X is a GIT-WORKING-COPY."
  (typep x 'git-working-copy))

(defun make-git-working-copy (store &key stamp-ref)
  "→ GIT-WORKING-COPY over STORE's worktree. STAMP-REF: NIL (the stamp is HEAD)
or a full ref name starting with \"refs/\" that holds the stamp. A bare STORE
→ REVISION-ERROR; a STAMP-REF that is not a full, valid ref name
(`check-ref-format`) → REVISION-ERROR."
  (declare (ignore store stamp-ref))
  (%not-implemented 'make-git-working-copy))

(defun working-copy-path (working-copy)
  "→ absolute directory pathname of WORKING-COPY's worktree top-level (its
store's GIT-STORE-PATH)."
  (declare (ignore working-copy))
  (%not-implemented 'working-copy-path))

(defmethod rev:target-revisions ((target git-working-copy))
  (%not-implemented 'rev:target-revisions))

(defmethod rev:target-stamp ((target git-working-copy) revisions)
  (declare (ignore revisions))
  (%not-implemented 'rev:target-stamp))

(defmethod rev:target-state ((target git-working-copy))
  (%not-implemented 'rev:target-state))

(defmethod rev:target-drift ((target git-working-copy) store)
  (declare (ignore store))
  (%not-implemented 'rev:target-drift))

(defmethod rev:capture-revision ((target git-working-copy) store
                                 &key (parents nil parents-p) message)
  (declare (ignore store parents parents-p message))
  (%not-implemented 'rev:capture-revision))

(defmethod rev:target-fidelities ((target git-working-copy))
  (%not-implemented 'rev:target-fidelities))

(defmethod rev:target-transactional-p ((target git-working-copy))
  (%not-implemented 'rev:target-transactional-p))
