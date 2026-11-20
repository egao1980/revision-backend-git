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
;;;
;;; `git replay` only reports refs/heads/* updates, so the chain's tip gets a
;;; temporary branch for the run (deleted afterwards); replay runs in print
;;; mode (`--ref-action=print` where this git has it) and the backend moves
;;; REF itself through MOVE-REF (CAS, V0 restarts).
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
  (uiop:ensure-directory-pathname
   (uiop:parse-native-namestring
    (%git-line store '("rev-parse" "--path-format=absolute" "--git-common-dir")))))

(defun %directory-designator (path)
  "PATH (string or pathname) → absolute directory pathname (Lisp cwd)."
  (uiop:ensure-directory-pathname
   (uiop:ensure-absolute-pathname (if (stringp path) (uiop:parse-native-namestring path) path)
                                  (uiop:getcwd))))

(defun %native-directory (dir)
  "Native namestring of the directory DIR without its trailing separator."
  (string-right-trim "/" (uiop:native-namestring dir)))

(defun add-worktree (store path designator &key branch)
  "`git worktree add` → a GIT-STORE on the new linked worktree at PATH (a
directory that is absent or empty), checked out at the commit DESIGNATOR
names. BRANCH NIL: detached HEAD. BRANCH a short name: a new branch
refs/heads/BRANCH created at that commit; an existing BRANCH → REF-CONFLICT
(:NAME BRANCH :EXPECTED NIL :ACTUAL its id), nothing created. A bare STORE
works (the new worktree is the only checkout). Signals what RESOLVE-REVISION
signals; a PATH that is not empty → REVISION-ERROR."
  (check-type store git-store)
  (check-type path (or string pathname))
  (check-type branch (or null string))
  (%check-designator designator)
  (let ((id (%resolve-id store designator))
        (dir (%directory-designator path)))
    (when branch
      (let ((current (%branch-target store branch)))
        (when current
          (error 'rev:ref-conflict :name branch :expected nil :actual current))))
    (let ((native (%native-directory dir)))
      (when (if (uiop:directory-exists-p dir)
                (or (uiop:directory-files dir) (uiop:subdirectories dir))
                (probe-file (uiop:parse-native-namestring native)))
        (error 'rev:revision-error
               :message (format nil "~a is not an empty directory" native)))
      (%git store (append (list "worktree" "add" "-q")
                          (if branch (list "-b" branch) (list "--detach"))
                          (list native id)))
      (make-git-store dir :git (%git-store-program store)
                          :env (%git-store-env store)
                          :permit-force (%git-store-permit-force store)))))

(defun list-worktrees (store)
  "→ fresh list of plists, one per worktree of STORE's repository, main
worktree first, from `git worktree list --porcelain -z`:
  :PATH      absolute directory pathname
  :HEAD      commit id, NIL when unborn (or for a bare main entry)
  :BRANCH    short branch name (\"main\"), NIL when detached
  :DETACHED  T / NIL       :BARE  T / NIL
  :LOCKED    NIL, T, or the reason string
  :PRUNABLE  NIL, T, or the reason string"
  (check-type store git-store)
  (mapcar (lambda (entry)
            (let ((entry (copy-list entry)))
              (setf (getf entry :path)
                    (uiop:ensure-directory-pathname
                     (uiop:parse-native-namestring (getf entry :path))))
              entry))
          (parse-worktree-list (%git store '("worktree" "list" "--porcelain" "-z")))))

(defun %same-directory-p (a b)
  "True when the existing directories A and B are the same (truenames)."
  (let ((ta (probe-file a)) (tb (probe-file b)))
    (and ta tb (equal (namestring ta) (namestring tb)))))

(defun remove-worktree (store path &key force)
  "`git worktree remove [--force] PATH` → T. A worktree with local changes
without FORCE → GIT-COMMAND-ERROR, nothing removed. The main worktree, or a
PATH that is no worktree of STORE's repository → REVISION-ERROR."
  (check-type store git-store)
  (check-type path (or string pathname))
  (let* ((dir (%directory-designator path))
         (entries (list-worktrees store))
         (hit (and (uiop:directory-exists-p dir)
                   (find-if (lambda (e) (%same-directory-p dir (getf e :path))) entries))))
    (cond ((null hit)
           (error 'rev:revision-error
                  :message (format nil "~a is no worktree of this repository"
                                   (%native-directory dir))))
          ((eq hit (first entries))
           (error 'rev:revision-error
                  :message (format nil "~a is the main worktree" (%native-directory dir)))))
    (%git store (append (list "worktree" "remove")
                        (when force (list "--force"))
                        (list (%native-directory (getf hit :path)))))
    (%git store '("worktree" "prune"))
    t))

(defun parse-worktree-list (string)
  "Pure: `git worktree list --porcelain -z` output → list of plists with the
keys LIST-WORKTREES documents (:PATH as a native namestring, not a
pathname). Records are attribute lines each ended by NUL, a record ended by
an extra NUL. Unknown attributes are ignored. Malformed input (no
\"worktree\" line first, a HEAD that is not hex, a truncated record) →
REVISION-ERROR."
  (check-type string string)
  (let ((n (length string))
        (nul (code-char 0)))
    (when (zerop n)
      (return-from parse-worktree-list '()))
    (unless (char= nul (char string (1- n)))
      (%malformed "worktree list" string))
    (let ((fields (butlast (%split-string string nul)))
          (out '()))
      (loop while fields
            do (let ((head (pop fields)))
                 (unless (and (> (length head) 9) (string= "worktree " head :end2 9))
                   (%malformed "worktree list" head))
                 (let ((entry (list :path (subseq head 9) :head nil :branch nil :detached nil
                                    :bare nil :locked nil :prunable nil)))
                   (loop
                     (when (null fields)
                       (%malformed "worktree list" head)) ; record not ended by NUL NUL
                     (let* ((field (pop fields))
                            (sp (position #\Space field))
                            (key (if sp (subseq field 0 sp) field))
                            (value (and sp (subseq field (1+ sp)))))
                       (when (string= field "")
                         (return))
                       (cond ((string= key "HEAD")
                              (unless (and value (member (length value) '(40 64)) (%hex-p value))
                                (%malformed "worktree list" field))
                              (setf (getf entry :head)
                                    (if (every (lambda (c) (char= c #\0)) value)
                                        nil
                                        (string-downcase value))))
                             ((string= key "branch")
                              (setf (getf entry :branch)
                                    (if (and value (> (length value) 11)
                                             (string= "refs/heads/" value :end2 11))
                                        (subseq value 11)
                                        value)))
                             ((string= key "detached") (setf (getf entry :detached) t))
                             ((string= key "bare") (setf (getf entry :bare) t))
                             ((string= key "locked") (setf (getf entry :locked) (or value t)))
                             ((string= key "prunable") (setf (getf entry :prunable) (or value t))))))
                   (push entry out))))
      (nreverse out))))

;;; ---- replay ----------------------------------------------------------

(defun git-replay-p (store)
  "True when STORE's git (probed version) is at least *REPLAY-GIT-VERSION*."
  (check-type store git-store)
  (not (%version< (%git-store-version store) (parse-git-version *replay-git-version*))))

(defun %replay-print-option (store)
  "(\"--ref-action=print\") when this git's replay has the option, else NIL
(older replay prints by default)."
  (multiple-value-bind (out code err) (%git store '("replay" "-h") :ok '(0 1 128 129))
    (declare (ignore code))
    (when (or (search "--ref-action" out) (search "--ref-action" err))
      (list "--ref-action=print"))))

(defun %check-chain (commits)
  "REVISION-ERROR unless COMMITS is a parents-first chain without merges."
  (loop for prev = nil then c
        for c in commits
        do (when (rest (rev:revision-parent-ids c))
             (error 'rev:revision-error
                    :message (format nil "~a is a merge commit: replay refuses merges"
                                     (rev:revision-id c))))
           (when (and prev (not (equal (rev:revision-parent-ids c) (list (rev:revision-id prev)))))
             (error 'rev:revision-error
                    :message (format nil "~a is not the child of ~a: not a chain, parents first"
                                     (rev:revision-id c) (rev:revision-id prev))))))

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
  (check-type store git-store)
  (let ((required *replay-git-version*))
    (when (%version< (%git-store-version store) (parse-git-version required))
      (error 'vc-tool-version-error :tool "git" :found (git-version store) :required required)))
  (check-type revisions list)
  (check-type ref (or null string))
  (when (null revisions)
    (return-from replay-revisions '()))
  (%check-designator onto)
  (when expected (%check-designator expected))
  (let* ((commits (mapcar (lambda (d) (%check-designator d) (%resolve store d)) revisions))
         (onto-id (%resolve-id store onto)))
    (%check-chain commits)
    (let* ((want (and ref (if expected-p
                              (and expected (%resolve-id store expected))
                              (%branch-target store ref))))
           (tip (rev:revision-id (car (last commits))))
           (base (first (rev:revision-parent-ids (first commits))))
           (temp (concatenate 'string "refs/heads/" (string-downcase (%temp-name "rbg-replay"))))
           (new-tip nil))
      (%git store (list "update-ref" temp tip (%zero-id store)))
      (unwind-protect
           (multiple-value-bind (out code)
               (%git store (append (list "replay")
                                   (%replay-print-option store)
                                   (list "--onto" onto-id
                                         (if base (format nil "~a..~a" base temp) temp)))
                     :ok '(0 1)
                     :extra-env (list (cons "GIT_COMMITTER_DATE"
                                            (format nil "@~d +0000" (%now-seconds)))))
             (when (eql code 1)
               (error 'replay-conflict :revision nil :onto onto-id))
             (setf new-tip
                   (or (loop for line in (%lines out)
                             for f = (%split-string line #\Space)
                             when (and (= 4 (length f)) (string= "update" (first f))
                                       (string= temp (second f)))
                               return (third f))
                       ;; A replay that updated the ref itself.
                       (let ((now (%rev-parse-verify store temp)))
                         (and now (not (equal now tip)) now))
                       (error 'git-command-error :command (list "replay" "--onto" onto-id)
                                                 :exit-code code
                                                 :stderr "git replay reported no new tip"))))
        (%git store (list "update-ref" "-d" temp) :ok '(0 1)))
      (let ((new '())
            (id new-tip))
        (dotimes (i (length commits))
          (declare (ignorable i))
          (let ((c (%commit store id)))
            (push c new)
            (setf id (first (rev:revision-parent-ids c)))))
        (when ref
          (rev:move-ref store ref new-tip :expected want))
        new))))
