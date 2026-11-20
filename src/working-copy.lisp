(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; GIT-WORKING-COPY — the store's worktree as a REVISION-PROTOCOL target.
;;;
;;; TARGET-STATE is the tree `git add -A` would record: tracked files as they
;;; are on disk plus untracked files that are not ignored (.gitignore,
;;; .git/info/exclude, core.excludesFile), read the same way as
;;; REVISION-STATE. The index does not count: staging a change neither causes
;;; nor clears drift. Computed through the store's stat index (below:
;;; GIT_INDEX_FILE = <git dir>/rbg-stat-index, `add -A`, `write-tree`), never
;;; the real one.
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

;;; ------------------------------------------------------------------
;;; The stat index (racy git). The worktree's tree id comes from `add -A` on
;;; a private, persistent index file per worktree, <git dir>/rbg-stat-index,
;;; that only %WORKTREE-TREE-ID uses. git keeps its stat cache, so `add -A`
;;; hashes only files whose stat changed since the last call, and git's own
;;; racy-git rule keeps that sound: an entry whose mtime is not older than
;;; the index file's mtime is compared by content, and racily clean entries
;;; are smudged when the index is written. The rule holds only while git
;;; alone writes the file in place, so the backend never copies an index
;;; with stat data over another, or touches one: a copy gets a fresh mtime
;;; and turns racy entries into trusted ones (#4: a copy of the real index
;;; made a same-size rewrite in the same second, inode reused, look clean). The
;;; real index follows the same rule: only git writes it (%SYNC-REAL-INDEX).
;;;
;;; `add -A` starts from the index it runs on: tracked files that match an
;;; ignore rule stay, and with core.fileMode / core.symlinks false modes come
;;; from the entries. So each call first makes the stat index's paths and
;;; modes match the real index (%SEED-STAT-INDEX; reseeded entries carry no
;;; stat data and are hashed once). Two threads of this process take a per-file lock;
;;; another process holding the index lock → the old way, a throwaway
;;; temporary index without stat data (every file hashed; correct, slower).
;;; ------------------------------------------------------------------

(defvar *stat-index-locks* (make-hash-table :test 'equal)
  "Native namestring of a stat index → its BT2 lock.")

(defvar *stat-index-locks-lock* (bt2:make-lock :name "revision-backend-git stat index table"))

(defun %stat-index-file (store)
  (merge-pathnames "rbg-stat-index" (git-store-git-dir store)))

(defun %stat-index-lock (file)
  (let ((key (uiop:native-namestring file)))
    (bt2:with-lock-held (*stat-index-locks-lock*)
      (or (gethash key *stat-index-locks*)
          (setf (gethash key *stat-index-locks*)
                (bt2:make-lock :name (format nil "stat index ~a" key)))))))

(defun %octet-records (octets)
  "OCTETS split at NUL bytes; the empty record after a final NUL dropped."
  (loop with n = (length octets)
        for start = 0 then (1+ end)
        while (< start n)
        for end = (or (position 0 octets :start start) n)
        collect (subseq octets start end)))

(defun %record-path (record)
  "Path octets of an `ls-files -s` RECORD (after the TAB)."
  (subseq record (1+ (position 9 record))))

(defun %join-records (records)
  "RECORDS (octet vectors) each followed by a NUL byte → one octet vector."
  (let ((out (make-array (reduce #'+ records :key (lambda (r) (1+ (length r))))
                         :element-type '(unsigned-byte 8)))
        (i 0))
    (dolist (r records out)
      (replace out r :start1 i)
      (incf i (length r))
      (setf (aref out i) 0)
      (incf i))))

(defun %record-mode (record)
  "Mode octets of an `ls-files -s` RECORD (before the first space)."
  (subseq record 0 (position 32 record)))

(defun %index-entries (store &key extra-env ignored)
  "`ls-files -z -s` of the real index (or of the index EXTRA-ENV names),
only the entries matching an ignore rule when IGNORED → a hash table path
octets → mode octets."
  (let ((table (make-hash-table :test 'equalp)))
    (dolist (r (%octet-records
                (%git store (append '("ls-files" "-z" "-s")
                                    (and ignored '("-c" "-i" "--exclude-standard")))
                      :output :octets :extra-env extra-env))
               table)
      (let ((path (%record-path r)))
        (unless (gethash path table)
          (setf (gethash path table) (%record-mode r)))))))

(defparameter +inherited-modes+
  (list (map '(vector (unsigned-byte 8)) #'char-code "100755")
        (map '(vector (unsigned-byte 8)) #'char-code "120000"))
  "Modes `add -A` may keep from an existing entry instead of reading them off
the disk (core.fileMode / core.symlinks false).")

(defun %seed-stat-index (store env)
  "Give the index ENV names the entries `add -A` would start from in a copy
of the real index, keeping the stat data of the rest: each real-index path
missing there or with another mode is (re)added from the real index (no
stat data, so it is hashed once); a path the real index lacks is removed
when it matches an ignore rule (a copy would not have it, and `add -A` does
not add it) or has a mode `add -A` could only have kept from an entry
(+INHERITED-MODES+), so `add -A` adds it afresh."
  (let* ((real-records (%octet-records (%git store '("ls-files" "-z" "-s") :output :octets)))
         (real (make-hash-table :test 'equalp))
         (have (%index-entries store :extra-env env))
         (have-ignored nil)
         (add '())
         (drop '()))
    (dolist (r real-records)
      (let ((path (%record-path r)))
        (unless (gethash path real)
          (setf (gethash path real) (%record-mode r)))))
    (dolist (r real-records)
      (let ((mode (gethash (%record-path r) have)))
        (unless (and mode (equalp mode (gethash (%record-path r) real)))
          (push r add))))
    (maphash (lambda (path mode)
               (unless (gethash path real)
                 (when (or (member mode +inherited-modes+ :test #'equalp)
                           (gethash path (or have-ignored
                                             (setf have-ignored
                                                   (%index-entries store :extra-env env :ignored t)))))
                   (push path drop))))
             have)
    (when drop
      (%git store '("update-index" "-z" "--force-remove" "--stdin")
            :extra-env env :input (%join-records drop)))
    (when add
      (%git store '("update-index" "-z" "--index-info")
            :extra-env env :input (%join-records (nreverse add))))))

(defun %index-lock-busy-p (condition)
  (let ((err (git-command-error-stderr condition)))
    (and err (search ".lock" err) (search "exists" err))))

(defun %worktree-tree-id/no-stat (store)
  "Tree id of `add -A` through a throwaway index holding the real index's
entries without stat data: every file is hashed."
  (%call-with-temp-index
   store nil
   (lambda (env)
     (%git store '("update-index" "-z" "--index-info")
           :extra-env env
           :input (%git store '("ls-files" "-s" "-z") :output :octets))
     (%git store '("add" "-A") :extra-env env)
     (%chomp (%git store '("write-tree") :extra-env env)))))

(defun %worktree-tree-id (store)
  "Tree id of what `git add -A` would record in STORE's worktree, through the
stat index; the real index is only read."
  (let* ((file (%stat-index-file store))
         (env (list (cons "GIT_INDEX_FILE" (uiop:native-namestring file)))))
    (or (block cached
          (handler-bind ((git-command-error
                           (lambda (c) (when (%index-lock-busy-p c) (return-from cached nil)))))
            (bt2:with-lock-held ((%stat-index-lock file))
              (%seed-stat-index store env)
              (%git store '("add" "-A") :extra-env env)
              (%chomp (%git store '("write-tree") :extra-env env)))))
        (%worktree-tree-id/no-stat store))))

(defun %working-tree-id (target)
  "Tree id of what `git add -A` would record in TARGET's worktree."
  (%worktree-tree-id (working-copy-store target)))

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
files of TREE are written (`checkout-index -f` from a temporary index
holding TREE), tracked and untracked files outside TREE are deleted, ignored
files (by the rules in effect after the write) are left alone; embedded
repositories are never entered. Only paths whose content or mode differs
from TREE are written (diff of the worktree's tree id, %WORKTREE-TREE-ID,
against TREE), so the stat caches of the other files stay valid. → TREE."
  (let ((top (git-store-path store))
        (current (%worktree-tree-id store)))
    (unless (equal current tree)
      (let ((changed (%octet-records
                      (%git store (list "diff-tree" "-r" "-z" "--no-renames" "--name-only"
                                        "--diff-filter=d" current tree)
                            :output :octets))))
        (%call-with-temp-index
         store nil
         (lambda (env)
           (%git store (list "read-tree" tree) :extra-env env)
           (when changed
             (%git store '("checkout-index" "-f" "-z" "--stdin")
                   :extra-env env :input (%join-records changed)))
           ;; The index holds exactly TREE: "others, not ignored" are precisely
           ;; the paths to delete.
           (dolist (rel (%nul-records (%git store '("ls-files" "-z" "--others" "--exclude-standard")
                                            :extra-env env)))
             (unless (char= #\/ (char rel (1- (length rel))))
               (%delete-worktree-path top rel)))))))
    tree))

(defmethod rev:target-transactional-p ((target git-working-copy))
  nil)
