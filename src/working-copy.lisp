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
;;; V1 (src/plan.lisp, src/projection.lisp, src/worktree.lisp): (SETF
;;; TARGET-STATE), PLAN-TRANSITION / EXECUTE-PLAN / PROJECT-PLAN,
;;; projections, linked worktrees. V1 changes TARGET-FIDELITIES to
;;; (:EXACT :SHADOW); CAPTURE-REVISION is unchanged.
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
  (check-type store git-store)
  (check-type stamp-ref (or null string))
  (when (git-store-bare-p store)
    (error 'rev:revision-error :message "a bare repository has no working copy"))
  (when stamp-ref
    (unless (and (> (length stamp-ref) 5) (string= "refs/" stamp-ref :end2 5)
                 (eql 0 (nth-value 1 (%git store (list "check-ref-format" stamp-ref)
                                           :ok '(0 1 128)))))
      (error 'rev:revision-error
             :message (format nil "~s is not a full, valid ref name" stamp-ref))))
  (make-instance 'git-working-copy :store store :stamp-ref (and stamp-ref (copy-seq stamp-ref))))

(defun working-copy-path (working-copy)
  "→ absolute directory pathname of WORKING-COPY's worktree top-level (its
store's GIT-STORE-PATH)."
  (git-store-path (working-copy-store working-copy)))

(defun %stamp-name (target)
  (or (working-copy-stamp-ref target) "HEAD"))

(defmethod rev:target-revisions ((target git-working-copy))
  (let ((id (%rev-parse-verify (working-copy-store target)
                               (concatenate 'string (%stamp-name target) "^{commit}"))))
    (and id (list id))))

(defun %stamp-id (x)
  (etypecase x
    (rev:revision (rev:revision-id x))
    (string (copy-seq x))))

(defmethod rev:target-stamp ((target git-working-copy) revisions)
  (check-type revisions list)
  (let ((store (working-copy-store target))
        (ids (remove-duplicates (mapcar #'%stamp-id revisions) :test #'equal :from-end t)))
    (cond
      ((rest ids)
       (error 'rev:revision-error
              :message (format nil "a git working copy holds one stamp, not ~s" ids)))
      ((null ids)
       (unless (working-copy-stamp-ref target)
         (error 'rev:revision-error :message "the HEAD stamp cannot be cleared"))
       (%git store (list "update-ref" "-d" (working-copy-stamp-ref target)) :ok '(0 1))
       nil)
      (t
       (let ((id (%resolve-id store (first ids))))
         (%git store (list "update-ref" (%stamp-name target) id))
         (list id))))))

(defun %working-tree-id (target)
  "Tree id of what `git add -A` would record, through a temporary index.
The temporary index holds the real index's entries (so tracked files that
match an ignore rule stay) but no stat data: `update-index --index-info`
records none, so `add -A` hashes every file. A copy of the real index would
carry its stat cache, and that cache lies after %WRITE-WORKTREE rewrote files
behind it: git compares whole-second mtimes, so a same-size rewrite in the
same second (with the inode reused, as ext4 does, or with no inode at all, as
on Windows) looks clean and `add -A` keeps the old blob."
  (let ((store (working-copy-store target)))
    (%call-with-temp-index
     store nil
     (lambda (env)
       (%git store '("update-index" "-z" "--index-info")
             :extra-env env
             :input (%git store '("ls-files" "-s" "-z") :output :octets))
       (%git store '("add" "-A") :extra-env env)
       (%chomp (%git store '("write-tree") :extra-env env))))))

(defmethod %operand-tree ((store git-store) (operand git-working-copy))
  (%working-tree-id operand))

(defmethod rev:target-state ((target git-working-copy))
  (%tree-state (working-copy-store target) (%working-tree-id target)))

(defmethod rev:target-drift ((target git-working-copy) store)
  (let* ((git (if (typep store 'git-store) store (working-copy-store target)))
         (stamp (first (rev:target-revisions target)))
         (stamp-tree (if stamp
                         (rev:revision-content (%resolve git stamp))
                         (%empty-tree git)))
         (tree (%working-tree-id target)))
    (unless (string= stamp-tree tree)
      (%diff-trees git stamp-tree tree :changes nil))))

(defmethod rev:capture-revision ((target git-working-copy) store
                                 &key (parents nil parents-p) message provenance
                                   valid-at id)
  (when id
    (error 'rev:revision-error :message "git chooses commit ids: :ID is not accepted"))
  (let* ((git (if (typep store 'git-store) store (working-copy-store target)))
         (tree (%working-tree-id target))
         (revision (rev:commit-revision git tree
                                        :parents (if parents-p parents (rev:target-revisions target))
                                        :message (or message "capture")
                                        :provenance provenance
                                        :valid-at valid-at)))
    (rev:target-stamp target (list revision))
    revision))

(defmethod rev:target-fidelities ((target git-working-copy))
  "(:EXACT :SHADOW): :EXACT is a virtual projection of the goal tree, :SHADOW
a linked worktree where the plan really runs (src/plan.lisp)."
  (list :exact :shadow))

;;; ------------------------------------------------------------------
;;; Writing a tree into a worktree (V1). Shared by (SETF TARGET-STATE),
;;; (SETF PROJECTION-STATE), DISCARD-DRIFT and the stamp-ref steps of
;;; EXECUTE-PLAN. Only a temporary index is used: the real index, HEAD and
;;; every ref stay as they are.
;;; ------------------------------------------------------------------

(defun %delete-worktree-path (top rel)
  "Delete the file REL (git's /-separated path relative to the directory TOP),
then each parent directory below TOP that this leaves empty."
  (let* ((top-native (uiop:native-namestring top))
         (comps (%split-string rel #\/)))
    (handler-case (delete-file (uiop:parse-native-namestring
                                (concatenate 'string top-native rel)))
      (file-error () nil))
    (loop for k from (1- (length comps)) downto 1
          for dir = (uiop:ensure-directory-pathname
                     (uiop:parse-native-namestring
                      (format nil "~a~{~a~^/~}" top-native (subseq comps 0 k))))
          while (and (uiop:directory-exists-p dir)
                     (null (uiop:directory-files dir))
                     (null (uiop:subdirectories dir)))
          do (uiop:delete-empty-directory dir))))

(defun %write-worktree (store tree)
  "Make the files of STORE's worktree equal the tree object TREE (an id):
files of TREE are written (`checkout-index -f -a` from a temporary index
holding TREE), tracked and untracked files outside TREE are deleted, ignored
files (by the rules in effect after the write) are left alone; embedded
repositories are never entered. → TREE."
  (let ((top (git-store-path store)))
    (%call-with-temp-index
     store nil
     (lambda (env)
       (%git store (list "read-tree" tree) :extra-env env)
       (%git store '("checkout-index" "-f" "-a") :extra-env env)
       ;; The index holds exactly TREE: "others, not ignored" are precisely the
       ;; paths to delete.
       (dolist (rel (%nul-records (%git store '("ls-files" "-z" "--others" "--exclude-standard")
                                        :extra-env env)))
         (unless (char= #\/ (char rel (1- (length rel))))
           (%delete-worktree-path top rel)))))
    tree))

(defmethod rev:target-transactional-p ((target git-working-copy))
  nil)
