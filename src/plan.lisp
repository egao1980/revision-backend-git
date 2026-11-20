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

(defun %id-tree (store id)
  "Tree id of the commit ID, the empty tree for NIL."
  (if id
      (rev:revision-content (%resolve store id))
      (%empty-tree store)))

;;; ---- plans ---------------------------------------------------------

(defun %toward (store id base)
  "Next id from ID toward BASE along one parent edge: the first parent that is
BASE or reaches it; BASE NIL → the first parent (NIL past the root)."
  (let ((parents (rev:revision-parent-ids (%resolve store id))))
    (if base
        (find-if (lambda (p) (rev:ancestor-p store base p)) parents)
        (first parents))))

(defun %path-to-base (store id base)
  "(ID … BASE), one parent edge per element; BASE NIL → ends with NIL."
  (loop for x = id then (%toward store x base)
        collect x
        until (or (null x) (equal x base))))

(defun %plan-steps (store direction ids)
  "PLAN-STEPs between consecutive elements of IDS (:SUMMARY step diffs)."
  (loop for tail on ids
        while (rest tail)
        collect (let ((from (first tail))
                      (to (second tail)))
                  (make-instance 'rev:plan-step
                                 :direction direction
                                 :from from
                                 :to to
                                 :diff (%diff-trees store (%id-tree store from) (%id-tree store to)
                                                    :summary nil)
                                 :reversible-p t))))

(defmethod rev:plan-transition ((store git-store) (target git-working-copy) to)
  (%check-designator to)
  (let ((start (rev:target-revisions target)))
    (when (rest start)
      (error 'rev:revision-error
             :message (format nil "target is stamped with several revisions ~s" start)))
    (let* ((goal (%resolve-id store to))
           (from (and start (%resolve-id store (first start))))
           (merge (and from (rev:merge-base store from goal)))
           (base (and merge (rev:revision-id merge)))
           (down (if from (%path-to-base store from base) (list nil)))
           (up (reverse (%path-to-base store goal base))))
      (make-instance 'rev:transition-plan
                     :store store
                     :start (copy-list start)
                     :goal (list goal)
                     :base base
                     :steps (append (%plan-steps store :down down)
                                    (%plan-steps store :up up))))))

(defmethod %operand-tree ((store git-store) (operand rev:transition-plan))
  (%id-tree store (first (rev:plan-goal operand))))

;;; ---- execution -----------------------------------------------------

(defun %remaining-steps (plan stamp)
  "Steps of PLAN still to run on a target stamped STAMP: all of them at
PLAN-START, those after the step whose TO is STAMP otherwise.
REVISION-ERROR for a stamp that is neither."
  (let ((steps (rev:plan-steps plan)))
    (if (equal stamp (rev:plan-start plan))
        steps
        (loop for tail on steps
              when (equal stamp (let ((to (rev:step-to (first tail)))) (and to (list to))))
                return (rest tail)
              finally (error 'rev:revision-error
                             :message (format nil "target stamp ~s is neither the plan's start nor any step's TO"
                                              stamp))))))

(defun %stamp-tree (store target)
  "Tree of TARGET's stamped revision (the empty tree when unstamped)."
  (%id-tree store (first (rev:target-revisions target))))

(defun %sync-real-index (ws tree)
  "HEAD stamp only: the real index of WS holds TREE, with fresh stat data
(`read-tree --reset` when it differs, then `update-index --refresh`), so that
`read-tree -m -u` sees the worktree as up to date. Only git writes the real
index, in place (never a copy over it), so git's racy-git rule covers its
stat cache: an entry not older than the index file is compared by content.
%WRITE-WORKTREE rewrites only the paths that differ, so the refresh hashes
those and trusts the rest."
  (unless (equal tree (%index-tree ws))
    (%git ws (list "read-tree" "--reset" tree)))
  (%git ws '("update-index" "-q" "--refresh") :ok '(0 1)))

(defun %run-step (target store step from-tree)
  "Apply STEP to TARGET's files and stamp its TO → the TO tree. FROM-TREE is
the tree the target holds now."
  (let* ((ws (working-copy-store target))
         (ref (working-copy-stamp-ref target))
         (to (rev:step-to step))
         (to-tree (%id-tree store to)))
    (cond (ref
           (%write-worktree ws to-tree)
           (if to
               (%git ws (list "update-ref" ref to))
               (%git ws (list "update-ref" "-d" ref) :ok '(0 1))))
          (t
           (%git ws (list "read-tree" "-m" "-u" from-tree to-tree))
           ;; TO NIL (down past a root): files only, HEAD cannot be unborn again.
           (when to
             (%git ws (list "update-ref" "-m"
                            (format nil "execute-plan: ~(~a~) to ~a" (rev:step-direction step) to)
                            "HEAD" to)))))
    to-tree))

(defun %rollback (target start start-tree)
  "Files and stamp of TARGET back to START (ids) / START-TREE."
  (let ((ws (working-copy-store target)))
    (%write-worktree ws start-tree)
    (cond ((working-copy-stamp-ref target)
           (rev:target-stamp target start))
          (t
           (%sync-real-index ws start-tree)
           (if start
               (%git ws (list "update-ref" "-m" "execute-plan: rollback" "HEAD" (first start)))
               (%git ws '("update-ref" "-d" "HEAD") :ok '(0 1)))))))

(defmethod rev:execute-plan ((target git-working-copy) plan)
  (check-type plan rev:transition-plan)
  (let* ((store (rev:plan-store plan))
         (ws (working-copy-store target))
         (start (rev:target-revisions target))
         (steps (%remaining-steps plan start))
         (skipped '()))
    (let ((drift (rev:target-drift target store)))
      (when drift
        (restart-case (error 'rev:target-drift-detected :target target :drift drift)
          (rev:capture-and-continue (&key id (message "captured drift"))
            :report "Capture the drifted state as a revision, re-plan from it and run that."
            (rev:capture-revision target store :id id :message message)
            (return-from rev:execute-plan
              (rev:execute-plan target (rev:plan-transition store target (first (rev:plan-goal plan))))))
          (rev:discard-drift ()
            :report "Reset the target's files to its stamped revision's state, then run."
            (%write-worktree ws (%stamp-tree store target)))
          (abort ()
            :report "Run nothing; return NIL."
            (return-from rev:execute-plan nil)))))
    (let* ((start-tree (%stamp-tree store target))
           (current start-tree))
      (unless (working-copy-stamp-ref target)
        (%sync-real-index ws start-tree))
      (dolist (step steps (values (rev:target-revisions target) (nreverse skipped)))
        (block one-step
          (loop for attempt from 1
                for cause = (handler-case (progn (setf current (%run-step target store step current))
                                                 nil)
                              ((or git-command-error file-error) (c) c))
                while cause
                do (restart-case (error 'rev:step-failed :step step :cause cause :attempt attempt)
                     (rev:retry-step ()
                       :report "Apply the step again."
                       nil)
                     (rev:skip-step ()
                       :report "Leave the files and the stamp; continue with the next step."
                       (push step skipped)
                       (return-from one-step))
                     (rev:rollback-to-start ()
                       :report "Restore the files and the stamp from before this run; return NIL."
                       (%rollback target start start-tree)
                       (return-from rev:execute-plan nil)))))))))

(defmethod (setf rev:target-state) (state (target git-working-copy))
  (let ((ws (working-copy-store target)))
    (%write-worktree ws (%content-tree ws state '()))
    state))

;;; ---- plan projections ----------------------------------------------

(defparameter +shadow-stamp-ref+ "refs/worktree/rbg-shadow"
  "Stamp ref of the working copy a :SHADOW projection runs its plan on:
per-worktree (refs/worktree/), so invisible to the main checkout and gone
with the linked worktree.")

(defun %shadow-projection (target plan seen)
  "Materialize PLAN-START's tree (drift left behind) and run PLAN there."
  (let* ((store (rev:plan-store plan))
         (start (rev:plan-start plan))
         (p (%materialize store (%id-tree store (first start)) (rev:plan-goal plan)
                          :fidelity :shadow :target target :seen seen))
         (done nil))
    (unwind-protect
         (let ((shadow (make-git-working-copy (working-copy-store (%projection-linked p))
                                              :stamp-ref +shadow-stamp-ref+)))
           (when start
             (rev:target-stamp shadow start))
           (rev:execute-plan shadow plan)
           (setf done t)
           p)
      (unless done
        (rev:discard-projection p)))))

(defun %plan-projection (target plan fidelity)
  "Projection of TARGET after PLAN at FIDELITY (:EXACT or :SHADOW),
stale-checked against TARGET's stamp and state now."
  (let ((seen (%target-snapshot target))
        (store (rev:plan-store plan)))
    (ecase fidelity
      (:exact
       (%virtual-projection store (%id-tree store (first (rev:plan-goal plan))) (rev:plan-goal plan)
                            :target target :seen seen))
      (:shadow
       (%shadow-projection target plan seen)))))

(defmethod rev:project-plan ((target git-working-copy) plan &key fidelity)
  (check-type fidelity (member nil :exact :shadow :model))
  (check-type plan rev:transition-plan)
  (let* ((supported (rev:target-fidelities target))
         (wanted (or fidelity (first supported))))
    (if (member wanted supported)
        (%plan-projection target plan wanted)
        (restart-case (error 'rev:projection-unsupported :fidelity wanted :supported supported)
          (rev:use-shadow ()
            :report "Project at :SHADOW fidelity instead (a linked worktree where the plan runs)."
            (%plan-projection target plan :shadow))
          (rev:apply-for-real ()
            :report "Execute the plan on the target, then project its state exactly."
            (when (rev:execute-plan target plan)
              (%virtual-projection (rev:plan-store plan) (%working-tree-id target)
                                   (rev:plan-goal plan))))))))
