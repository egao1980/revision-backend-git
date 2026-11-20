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
;;; and an index hash are unchanged): every index written is a temporary
;;; GIT_INDEX_FILE or a linked worktree's own index, every file written lies
;;; under a linked worktree.
;;;
;;; A materialized projection keeps a GIT-WORKING-COPY over a GIT-STORE opened
;;; on its linked worktree; DISCARD-PROJECTION closes that store (its cat-file
;;; process), removes the worktree and prunes. Projections made by
;;; PROJECT-PLAN remember the target's stamp and state (TARGET / SEEN) and
;;; signal PROJECTION-STALE once either changed.
;;; ------------------------------------------------------------------

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
  ((store :initarg :store :reader %projection-store)
   (tree :initarg :tree :initform nil :reader %projection-tree-id
         :documentation "Virtual: the tree id. Materialized: the tree checked out at creation.")
   (parents :initarg :parents :initform '() :reader %projection-parents)
   (fidelity :initarg :fidelity :initform :exact :reader %projection-fidelity)
   (conflicts :initarg :conflicts :initform '() :reader %projection-conflicts)
   (stages :initarg :stages :initform '() :reader %projection-stages
           :documentation "PROJECT-MERGE: ((path s1 s2 s3) …), sN = (mode . oid) or NIL.")
   (marker-tree :initarg :marker-tree :initform nil :reader %projection-marker-tree)
   (worktree :initarg :worktree :initform nil :accessor %projection-worktree)
   (linked :initarg :linked :initform nil :accessor %projection-linked
           :documentation "Materialized: GIT-WORKING-COPY over the linked worktree's store.")
   (target :initarg :target :initform nil :reader %projection-target
           :documentation "PROJECT-PLAN: the GIT-WORKING-COPY projected.")
   (seen :initarg :seen :initform nil :reader %projection-seen
         :documentation "PROJECT-PLAN: (stamp-ids . worktree-tree) of TARGET when projected.")
   (discarded :initform nil :accessor %projection-discarded))
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
  (check-type store git-store)
  (check-type tree string)
  (let ((id (%rev-parse-verify store tree)))
    (unless (and id (equal "tree" (%object-type store id)))
      (error 'rev:revision-error :message (format nil "~s names no tree object" tree)))
    (make-instance 'revision-filesystem
                   :store store
                   :revision nil
                   :tree id
                   :mtime (encode-universal-time 0 0 0 1 1 1970 0))))

(defgeneric revision-filesystem-tree (filesystem)
  (:documentation "→ the tree id a REVISION-FILESYSTEM reads (for a commit's
filesystem: that commit's tree)."))

(defmethod revision-filesystem-tree ((fs revision-filesystem))
  (or (%fs-tree fs)
      (rev:revision-content (%resolve (revision-filesystem-store fs)
                                      (revision-filesystem-revision fs)))))

;;; ---- internals -----------------------------------------------------

(defun %target-snapshot (target)
  "(stamp-ids . worktree-tree) of the GIT-WORKING-COPY TARGET."
  (cons (rev:target-revisions target) (%working-tree-id target)))

(defun %live (p)
  "P, after checking it may still be read: discarded → REVISION-ERROR; a
target projection whose target's stamp or state changed → PROJECTION-STALE."
  (when (%projection-discarded p)
    (error 'rev:revision-error :message (format nil "~a was discarded" p)))
  (let ((target (%projection-target p)))
    (when target
      (let ((seen (%projection-seen p)))
        (unless (and (equal (car seen) (rev:target-revisions target))
                     (equal (cdr seen) (%working-tree-id target)))
          (error 'rev:projection-stale :projection p)))))
  p)

(defun %projection-tree (p)
  "Tree id of P's state (live-checked). Materialized: `add -A` of its worktree."
  (%live p)
  (let ((linked (%projection-linked p)))
    (if linked
        (%working-tree-id linked)
        (%projection-tree-id p))))

(defun %virtual-projection (store tree parents &rest initargs)
  (apply #'make-instance 'git-projection :store store :tree tree
                                         :parents (copy-list parents) initargs))

(defun %projection-root ()
  "Absolute directory under which materialized projections go."
  (let ((d *projection-directory*))
    (uiop:ensure-directory-pathname
     (uiop:ensure-absolute-pathname
      (etypecase d
        (null (uiop:temporary-directory))
        (string (uiop:parse-native-namestring d))
        (pathname d))
      (uiop:getcwd)))))

(defun %anchor-commit (store parents)
  "A commit `worktree add --detach` can start from: the first parent, else a
parentless commit of the empty tree (dangling, no ref)."
  (or (first parents)
      (%commit-tree store (%empty-tree store) '() "projection anchor" nil nil)))

(defun %remove-linked-worktree (store dir linked-store)
  "Close LINKED-STORE, remove the linked worktree DIR, delete it, prune."
  (when linked-store
    (close-git-store linked-store))
  (when dir
    (%git store (list "worktree" "remove" "--force" (uiop:native-namestring dir))
          :ok '(0 1 128))
    (uiop:delete-directory-tree (uiop:ensure-directory-pathname dir)
                                :validate t :if-does-not-exist :ignore))
  (%git store '("worktree" "prune"))
  nil)

(defun %materialize (store tree parents &rest initargs)
  "→ a materialized GIT-PROJECTION: a fresh detached linked worktree of STORE
under (%PROJECTION-ROOT) holding TREE (`worktree add --no-checkout`, then
`read-tree -u --reset` with the linked worktree's own index). INITARGS go to
MAKE-INSTANCE (:FIDELITY, :TARGET, :SEEN)."
  (let* ((root (ensure-directories-exist (%projection-root)))
         (dir (uiop:ensure-directory-pathname
               (merge-pathnames (%temp-name "rbg-projection") root)))
         (linked nil)
         (done nil))
    (%git store (list "worktree" "add" "-q" "--detach" "--no-checkout"
                      (uiop:native-namestring dir) (%anchor-commit store parents)))
    (unwind-protect
         (progn
           (setf linked (make-git-store dir :git (%git-store-program store)
                                            :env (%git-store-env store)
                                            :permit-force (%git-store-permit-force store)))
           (%git linked (list "read-tree" "-u" "--reset" tree))
           (prog1 (apply #'make-instance 'git-projection
                         :store store :tree tree :parents (copy-list parents)
                         :worktree (git-store-path linked)
                         :linked (make-git-working-copy linked)
                         initargs)
             (setf done t)))
      (unless done
        (%remove-linked-worktree store dir linked)))))

(defun %nul-fields (string)
  "STRING split at NUL, empty fields dropped."
  (remove "" (%split-string string (code-char 0)) :test #'string=))

;;; ---- merges --------------------------------------------------------

(defun %merge-stages (entries)
  "merge-tree's conflicted file info (\"MODE OID STAGE<TAB>PATH\" each) →
((path s1 s2 s3) …) in first-seen order, sN = (mode . oid) or NIL."
  (let ((order '())
        (table (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let* ((tab (position #\Tab e))
             (meta (and tab (%split-string (subseq e 0 tab) #\Space))))
        (unless (and tab (= 3 (length meta)))
          (%malformed "merge-tree" e))
        (destructuring-bind (mode oid stage) meta
          (let ((path (subseq e (1+ tab)))
                (n (%digits "merge-tree" e stage)))
            (unless (<= 1 n 3)
              (%malformed "merge-tree" e))
            (unless (nth-value 1 (gethash path table))
              (push path order)
              (setf (gethash path table) (list nil nil nil)))
            (setf (nth (1- n) (gethash path table))
                  (cons (%octal-field "merge-tree" e mode) oid))))))
    (loop for path in (nreverse order)
          collect (cons path (gethash path table)))))

(defun %tree-with-stage (store tree stages n)
  "TREE with every path of STAGES set to its stage N entry (removed when that
stage is absent) → tree id. A temporary index; every blob already exists."
  (%call-with-temp-index
   store nil
   (lambda (env)
     (%git store (list "read-tree" tree) :extra-env env)
     (%git store '("update-index" "-z" "--index-info")
           :extra-env env
           :input (with-output-to-string (s)
                    (dolist (e stages)
                      (let ((entry (nth n e)))
                        (if entry
                            (format s "~o ~a~c~a~c" (car entry) (cdr entry) #\Tab (first e) (code-char 0))
                            (format s "0 ~a~c~a~c" (%zero-id store) #\Tab (first e) (code-char 0)))))))
     (%chomp (%git store '("write-tree") :extra-env env)))))

(defun %stage-node (store stage)
  (and stage
       (if (eql (car stage) #o160000)
           (diff:make-tree-file (cdr stage) :type :gitlink :mode #o160000)
           (%blob-file store (cdr stage) (car stage)))))

(defun %stage-conflict (store entry)
  "DIFF-CONFLICT of one conflicted path: ours = stage 1 → 2, theirs = 1 → 3."
  (destructuring-bind (path s1 s2 s3) entry
    (let ((names (%split-string path #\/)))
      (flet ((change (from to)
               (%file-change (cond ((null from) :add) ((null to) :remove) (t :modify))
                             names nil
                             :before-fn (lambda () (%stage-node store from))
                             :after-fn (lambda () (%stage-node store to)))))
        (make-instance 'diff:diff-conflict
                       :ours (change s1 s2)
                       :theirs (change s1 s3)
                       :footprint (list (%tree-fp names)))))))

;;; ---- patches -------------------------------------------------------

(defun %apply-patch-text (store projection tree patch)
  "`git apply --cached` PATCH onto TREE in a temporary index → the new tree id.
Refused → PATCH-REJECTED (the --check run writes nothing)."
  (%call-with-temp-index
   store nil
   (lambda (env)
     (%git store (list "read-tree" tree) :extra-env env)
     (multiple-value-bind (out code err)
         (%git store '("apply" "--cached" "--check" "-") :input patch :ok '(0 1 128)
                                                         :extra-env env)
       (declare (ignore out))
       (unless (eql code 0)
         (error 'patch-rejected :projection projection :stderr err)))
     (%git store '("apply" "--cached" "-") :input patch :extra-env env)
     (%chomp (%git store '("write-tree") :extra-env env)))))

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
  (check-type projection git-projection)
  (check-type patch (or string (vector (unsigned-byte 8)) diff:diff))
  (let* ((store (%projection-store projection))
         (tree (if (typep patch 'diff:diff)
                   (%write-tree store (values (diff:apply-diff (rev:projection-state projection)
                                                               patch)))
                   (%apply-patch-text store projection (%projection-tree projection)
                                      (if (stringp patch) patch (%octets patch))))))
    (%virtual-projection store tree (%projection-parents projection))))

;;; ---- revision-protocol methods -------------------------------------

(defmethod rev:project-state ((store git-store) designator &key fidelity writable)
  (declare (ignore fidelity))           ; git trees are exact
  (multiple-value-bind (tree parents)
      (typecase designator
        (git-working-copy
         (values (%working-tree-id designator) (rev:target-revisions designator)))
        (git-projection
         (values (%projection-tree designator) (rev:projection-parents designator)))
        (t
         (%check-designator designator)
         (let ((r (%resolve store designator)))
           (values (rev:revision-content r) (list (rev:revision-id r))))))
    (if writable
        (%materialize store tree parents)
        (%virtual-projection store tree parents))))

(defmethod rev:project-merge ((store git-store) a b)
  (%check-designator a)
  (%check-designator b)
  (let* ((ia (%resolve-id store a))
         (ib (%resolve-id store b))
         (fields (%nul-fields (%git store (list "merge-tree" "--write-tree" "-z" "--no-messages"
                                                "--allow-unrelated-histories" ia ib)
                                    :ok '(0 1))))
         (marker (first fields))
         (stages (%merge-stages (rest fields)))
         (base (if stages (%tree-with-stage store marker stages 1) marker))
         (conflicts (mapcar (lambda (e) (%stage-conflict store e)) stages)))
    (values (%virtual-projection store base (list ia ib)
                                 :marker-tree marker :conflicts conflicts :stages stages)
            (copy-list conflicts))))

(defmethod rev:projection-fidelity ((p git-projection))
  (%projection-fidelity p))

(defmethod rev:projection-view ((p git-projection))
  (%live p)
  (if (%projection-linked p)
      (pathlib:make-path (%projection-worktree p) :filesystem (pathlib:make-local-filesystem))
      (let ((fs (make-tree-filesystem (%projection-store p) (%projection-tree-id p))))
        (pathlib:make-path (pathlib:fs-parse fs "/") :filesystem fs))))

(defmethod rev:projection-state ((p git-projection))
  (%tree-state (%projection-store p) (%projection-tree p)))

(defmethod (setf rev:projection-state) (state (p git-projection))
  (%live p)
  (let ((linked (%projection-linked p)))
    (unless linked
      (error 'rev:revision-error
             :message (format nil "~a is not writable (a virtual projection)" p)))
    (%write-worktree (working-copy-store linked)
                     (%content-tree (%projection-store p) state '()))
    state))

(defmethod rev:projection-parents ((p git-projection))
  (copy-list (%projection-parents p)))

(defmethod rev:projection-conflicts ((p git-projection))
  (copy-list (%projection-conflicts p)))

(defmethod rev:projection-writable-p ((p git-projection))
  (and (%projection-linked p) t))

(defmethod rev:projection-discarded-p ((p git-projection))
  (%projection-discarded p))

(defmethod rev:commit-projection ((store git-store) (p git-projection)
                                  &key (parents nil parents-p) message ref provenance valid-at id)
  (let ((tree (%projection-tree p)))     ; discarded / stale first
    (when (%projection-conflicts p)
      (restart-case (error 'rev:merge-conflict :conflicts (rev:projection-conflicts p)
                                               :projection p)
        (rev:take-ours ()
          :report "Resolve every conflicted path to our side's file (removal included)."
          (setf tree (%tree-with-stage store tree (%projection-stages p) 2)))
        (rev:take-theirs ()
          :report "Resolve every conflicted path to their side's file (removal included)."
          (setf tree (%tree-with-stage store tree (%projection-stages p) 3)))
        (rev:use-merged ()
          :report "Commit the projected state (base values at the conflicted paths)."
          nil)
        (abort ()
          :report "Commit nothing; return NIL."
          (return-from rev:commit-projection nil))))
    (rev:commit-revision store tree
                         :parents (if parents-p parents (rev:projection-parents p))
                         :message message :ref ref :provenance provenance
                         :valid-at valid-at :id id)))

(defmethod rev:discard-projection ((p git-projection))
  (unless (%projection-discarded p)
    (let ((linked (%projection-linked p)))
      (when linked
        (%remove-linked-worktree (%projection-store p) (%projection-worktree p)
                                 (working-copy-store linked))))
    (setf (%projection-linked p) nil
          (%projection-worktree p) nil
          (%projection-discarded p) t))
  t)

(defmethod git-projection-tree ((p git-projection))
  (%projection-tree p))

(defmethod git-projection-worktree ((p git-projection))
  (%projection-worktree p))

(defmethod merge-projection-marker-tree ((p git-projection))
  (%projection-marker-tree p))

(defmethod %operand-tree ((store git-store) (operand git-projection))
  (%projection-tree operand))
