(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; V1 — plans and the working copy as a writable target.
;;;
;;; PLAN-TRANSITION (git-store, git-working-copy) → a REVISION-PROTOCOL
;;; TRANSITION-PLAN with the protocol's path rule (down to the merge-base,
;;; up to TO, one parent edge per step, first parent that is or reaches the
;;; base at a merge). Step diffs are STATE-DIFF at level :SUMMARY (file-level,
;;; lazy nodes; they apply and invert like every git diff). Every step is
;;; reversible: IRREVERSIBLE-STEP is never signalled by this backend.
;;; DESCRIBE-PLAN and STEP-FOOTPRINT are revision-protocol's own methods.
;;;
;;; EXECUTE-PLAN (git-working-copy) follows the protocol (drift check with
;;; CAPTURE-AND-CONTINUE / DISCARD-DRIFT / ABORT, a stamp after every step,
;;; resume from a step's TO, STEP-FAILED with RETRY-STEP / SKIP-STEP /
;;; ROLLBACK-TO-START).
;;;   HEAD stamp:  each step is `read-tree -m -u FROM-TREE TO-TREE` on the
;;;                real index, then `update-ref HEAD TO` (net effect of a
;;;                plan: `git reset --keep GOAL`; the current branch moves).
;;;   stamp ref:   worktree files only, through a temporary index; HEAD,
;;;                branches and the real index are never touched.
;;;   Ignored files are never touched, in both modes.
;;;
;;; (SETF TARGET-STATE) writes the worktree files so that TARGET-STATE equals
;;; STATE afterwards (tracked and untracked non-ignored files outside STATE
;;; are deleted, ignored files are left alone); HEAD, stamp ref and real
;;; index are untouched, so the target drifts.
;;;
;;; TARGET-FIDELITIES (git-working-copy) → (:EXACT :SHADOW).
;;; PROJECT-PLAN :EXACT → a virtual projection of the goal's tree (what
;;; EXECUTE-PLAN leaves on a target without drift); :SHADOW → a
;;; materialized projection (a linked worktree checked out at PLAN-START —
;;; drift left behind, as after DISCARD-DRIFT — where the plan's steps really
;;; run); :MODEL → PROJECTION-UNSUPPORTED (restarts
;;; USE-SHADOW and APPLY-FOR-REAL only). A target projection read or
;;; committed after the working copy's stamp or state changed →
;;; PROJECTION-STALE.
;;; ------------------------------------------------------------------

(defmethod rev:plan-transition ((store git-store) (target git-working-copy) to)
  (declare (ignore to))
  (%not-implemented 'plan-transition))

(defmethod rev:execute-plan ((target git-working-copy) plan)
  (declare (ignore plan))
  (%not-implemented 'execute-plan))

(defmethod rev:project-plan ((target git-working-copy) plan &key fidelity)
  (declare (ignore plan fidelity))
  (%not-implemented 'project-plan))

(defmethod (setf rev:target-state) (state (target git-working-copy))
  (declare (ignore state))
  (%not-implemented '(setf target-state)))

(defmethod %operand-tree ((store git-store) (operand rev:transition-plan))
  (%not-implemented 'state-diff-of-a-plan))
