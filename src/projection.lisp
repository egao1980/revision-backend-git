(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; V1 — projections over git (contract: cl-stack
;;; docs/capabilities/revision-backend-git.md, section V1).
;;;
;;; A GIT-PROJECTION is one of
;;;   virtual      a tree object in the repository (PROJECT-STATE without
;;;                :WRITABLE, PROJECT-MERGE, APPLY-PATCH, PROJECT-PLAN :EXACT).
;;;                PROJECTION-VIEW is a PATH on a read-only tree filesystem.
;;;   materialized a detached linked worktree under *PROJECTION-DIRECTORY*
;;;                (PROJECT-STATE :WRITABLE T, PROJECT-PLAN :SHADOW).
;;;                PROJECTION-VIEW is a PATH on the local filesystem;
;;;                PROJECTION-STATE is what `git add -A` would record there
;;;                (through a temporary index).
;;; Every fidelity is :EXACT except PROJECT-PLAN :SHADOW.
;;;
;;; None of PROJECT-STATE, PROJECT-MERGE, APPLY-PATCH, COMMIT-PROJECTION
;;; touches the store's worktree files or its real index (acceptance: status
;;; and an index hash are unchanged).
;;;
;;; Everything below is a stub: the implementation removes %NOT-IMPLEMENTED
;;; and every call to it.
;;; ------------------------------------------------------------------

(defun %not-implemented (what)
  (error "revision-backend-git: ~A is not implemented yet (V1)" what))

(defvar *projection-directory* nil
  "Directory (pathname or native string) under which materialized projections
(linked worktrees) are created, one fresh subdirectory each. NIL: the OS
temporary directory. Must not lie inside the store's worktree.")

(define-condition patch-rejected (rev:revision-error)
  ((projection :initarg :projection :reader patch-rejected-projection :initform nil)
   (stderr :initarg :stderr :reader patch-rejected-stderr :initform nil))
  (:report (lambda (c s)
             (format s "the patch does not apply to ~a~@[: ~a~]"
                     (patch-rejected-projection c)
                     (let ((e (patch-rejected-stderr c)))
                       (and e (plusp (length e))
                            (string-right-trim '(#\Newline #\Return) e))))))
  (:documentation "APPLY-PATCH: `git apply --cached` refused the patch text.
PROJECTION is the source projection (unchanged), STDERR git's message.
Nothing was written."))

(defclass git-projection (rev:projection)
  ((store :initarg :store :reader %projection-store))
  (:documentation "A REVISION-PROTOCOL projection over a GIT-STORE: a tree object
(virtual) or a detached linked worktree (materialized). See the comment at the
top of src/projection.lisp."))

(defun git-projection-p (x)
  "T when X is a GIT-PROJECTION."
  (typep x 'git-projection))

(defgeneric git-projection-tree (projection)
  (:documentation "→ the tree id (string) of PROJECTION-STATE. Materialized
projections write the tree of their worktree first (`add -A` into a
temporary index, `write-tree`). Discarded → REVISION-ERROR; stale →
PROJECTION-STALE."))

(defgeneric git-projection-worktree (projection)
  (:documentation "→ absolute directory pathname of a materialized projection's
linked worktree, NIL for a virtual one (and after DISCARD-PROJECTION)."))

(defgeneric merge-projection-marker-tree (projection)
  (:documentation "→ for a PROJECT-MERGE projection, the tree id `git merge-tree
--write-tree` produced (conflict markers in conflicted text files); NIL for
other projections."))

(defun make-tree-filesystem (store tree)
  "→ REVISION-FILESYSTEM over the tree object TREE (an id string) of STORE.
REVISION-FILESYSTEM-REVISION is NIL, REVISION-FILESYSTEM-TREE is TREE, every
FS-LAST-MODIFIED is the Unix epoch (universal time 2208988800). Reads as a
commit's filesystem does. A TREE that names no tree → REVISION-ERROR."
  (declare (ignore store tree))
  (%not-implemented 'make-tree-filesystem))

(defgeneric revision-filesystem-tree (filesystem)
  (:documentation "→ the tree id a REVISION-FILESYSTEM reads (for a commit's
filesystem: that commit's tree)."))

(defmethod revision-filesystem-tree ((fs revision-filesystem))
  (%not-implemented 'revision-filesystem-tree))

(defun apply-patch (projection patch)
  "→ a new virtual GIT-PROJECTION: PROJECTION's state with PATCH applied;
PROJECTION-PARENTS as PROJECTION's, fidelity :EXACT, not writable, no
conflicts. PROJECTION is unchanged.
PATCH: a string or octet vector in `git diff` format (`--binary` for binary
files) — applied with `git apply --cached` to a temporary index holding
PROJECTION's tree, then `write-tree`; a patch git refuses signals
PATCH-REJECTED and nothing is written. Or a DIFF-PROTOCOL:DIFF — applied with
DIFF-PROTOCOL:APPLY-DIFF to PROJECTION-STATE (its APPLY-CONFLICT and
restarts propagate). A discarded PROJECTION → REVISION-ERROR."
  (declare (ignore projection patch))
  (%not-implemented 'apply-patch))

;;; ---- revision-protocol methods -------------------------------------

(defmethod rev:project-state ((store git-store) designator &key fidelity writable)
  (declare (ignore designator fidelity writable))
  (%not-implemented 'project-state))

(defmethod rev:project-merge ((store git-store) a b)
  (declare (ignore a b))
  (%not-implemented 'project-merge))

(defmethod rev:projection-fidelity ((p git-projection))
  (%not-implemented 'projection-fidelity))

(defmethod rev:projection-view ((p git-projection))
  (%not-implemented 'projection-view))

(defmethod rev:projection-state ((p git-projection))
  (%not-implemented 'projection-state))

(defmethod (setf rev:projection-state) (state (p git-projection))
  (declare (ignore state))
  (%not-implemented '(setf projection-state)))

(defmethod rev:projection-parents ((p git-projection))
  (%not-implemented 'projection-parents))

(defmethod rev:projection-conflicts ((p git-projection))
  (%not-implemented 'projection-conflicts))

(defmethod rev:projection-writable-p ((p git-projection))
  (%not-implemented 'projection-writable-p))

(defmethod rev:projection-discarded-p ((p git-projection))
  (%not-implemented 'projection-discarded-p))

(defmethod rev:commit-projection ((store git-store) (p git-projection)
                                  &key parents message ref provenance valid-at id)
  (declare (ignore parents message ref provenance valid-at id))
  (%not-implemented 'commit-projection))

(defmethod rev:discard-projection ((p git-projection))
  (%not-implemented 'discard-projection))

(defmethod git-projection-tree ((p git-projection))
  (%not-implemented 'git-projection-tree))

(defmethod git-projection-worktree ((p git-projection))
  (%not-implemented 'git-projection-worktree))

(defmethod merge-projection-marker-tree ((p git-projection))
  (%not-implemented 'merge-projection-marker-tree))

(defmethod %operand-tree ((store git-store) (operand git-projection))
  (%not-implemented 'state-diff-of-a-projection))
