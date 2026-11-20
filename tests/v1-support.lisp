(in-package #:revision-backend-git/tests)

;;; V1 test support. The plan's acceptance line, checked by every V1 file:
;;; PROJECT-STATE, PROJECT-MERGE, COMMIT-PROJECTION, CAPTURE-REVISION (and
;;; APPLY-PATCH, PROJECT-PLAN) leave the store's worktree and its real index
;;; byte-identical — `git status` output, a hash of .git/index, and the bytes
;;; of every worktree file are the same before and after.

(defun quiet-status (repo)
  "Real `git status --porcelain=v2 -z --untracked-files=all --ignored`, with
GIT_OPTIONAL_LOCKS=0 so the observation itself never refreshes the index."
  (git* repo '("status" "--porcelain=v2" "-z" "--untracked-files=all" "--ignored")
        :env '(("GIT_OPTIONAL_LOCKS" . "0"))))

(defun index-hash (repo)
  "Real `git hash-object --no-filters` of REPO's .git/index, or :NO-INDEX."
  (let ((index (merge-pathnames ".git/index" (trepo-dir repo))))
    (if (probe-file index)
        (git repo "hash-object" "--no-filters" (uiop:native-namestring index))
        :no-index)))

(defun checkout-observation (repo)
  "(status index-hash worktree-bytes HEAD-symbolic) of REPO's main checkout."
  (list (quiet-status repo)
        (index-hash repo)
        (worktree-snapshot repo)
        (git* repo '("symbolic-ref" "-q" "HEAD") :ok '(0 1))))

(defmacro with-checkout-unchanged ((repo &optional (what "the operation")) &body body)
  "Run BODY → its values, then check REPO's status, index hash, worktree bytes
and HEAD are what they were before."
  (let ((before (gensym "BEFORE")) (after (gensym "AFTER")) (label (gensym "LABEL")))
    `(let ((,before (checkout-observation ,repo))
           (,label ,what))
       (multiple-value-prog1 (progn ,@body)
         (let ((,after (checkout-observation ,repo)))
           (ok (equal (first ,before) (first ,after))
               (format nil "~a: git status unchanged" ,label))
           (ok (equal (second ,before) (second ,after))
               (format nil "~a: index hash unchanged" ,label))
           (ok (equalp (third ,before) (third ,after))
               (format nil "~a: worktree byte-identical" ,label))
           (ok (equal (fourth ,before) (fourth ,after))
               (format nil "~a: HEAD unchanged" ,label)))))))

(defun tree-of (repo spec)
  "Real git: the tree id of the commit SPEC."
  (git repo "rev-parse" "--verify" (concatenate 'string spec "^{tree}")))

(defun tree-paths (repo tree)
  "Real git: every file path of TREE (ls-tree -r --name-only), in git order."
  (git-lines repo "ls-tree" "-r" "--name-only" tree))

(defun blob-text (repo tree path)
  "Real git: the content of PATH in TREE, as text."
  (from-utf8 (git* repo (list "cat-file" "blob" (format nil "~a:~a" tree path))
                   :output :octets)))

(defun worktree-paths (repo)
  "Real git: the truenames of every worktree of REPO (main first)."
  (loop for field in (uiop:split-string (git* repo '("worktree" "list" "--porcelain" "-z"))
                                        :separator (list (code-char 0)))
        when (and (> (length field) 9) (string= "worktree " field :end2 9))
          collect (namestring (truename (uiop:ensure-directory-pathname
                                         (uiop:parse-native-namestring (subseq field 9)))))))

(defun pathname-under-p (inner outer)
  "True when the directory INNER is OUTER or below it (truenames)."
  (let ((i (namestring (truename inner))) (o (namestring (truename outer))))
    (and (>= (length i) (length o)) (string= o i :end2 (length o)))))

(defun real-merge-tree (repo a b)
  "Real `git merge-tree --write-tree -z --name-only --no-messages
--allow-unrelated-histories A B` → (values tree-id conflicted-paths exit-code)."
  (multiple-value-bind (out code)
      (git* repo (list "merge-tree" "--write-tree" "-z" "--name-only" "--no-messages"
                       "--allow-unrelated-histories" a b)
            :ok '(0 1))
    (let ((fields (uiop:split-string out :separator (list (code-char 0)))))
      (values (first fields)
              (remove-duplicates (remove "" (rest fields) :test #'string=)
                                 :test #'string= :from-end t)
              code))))

(defun conflict-paths (conflicts)
  "Sorted path strings (\"a/b.txt\") of DIFF-CONFLICTs, from their changes."
  (sort (remove-duplicates
         (loop for c in conflicts
               for change = (or (diff:conflict-ours c) (diff:conflict-theirs c))
               collect (format nil "~{~a~^/~}" (diff:change-path change)))
         :test #'string=)
        #'string<))

;;; ---- fixtures -----------------------------------------------------------

(defparameter +shared-base+ (lines "1" "2" "3" "4" "5" "6" "7" "8"))

(defun build-diverged (repo &key conflict)
  "base on main; topic: base ← t1; main: base ← o1. Checked out: main (o1).
base: shared.txt (8 lines), md.txt, keep.txt.
o1 (main):  shared.txt line 1 → \"ours\"; ours.txt added; md.txt edited.
t1 (topic): shared.txt line 8 → \"theirs\" (CONFLICT: line 1 → \"theirs\");
            theirs.txt added; md.txt unchanged (CONFLICT: md.txt deleted).
→ (:base id :ours id :theirs id)."
  (put-file repo "shared.txt" +shared-base+)
  (put-file repo "md.txt" (lines "md"))
  (put-file repo "keep.txt" (lines "keep"))
  (let ((base (commit-all repo "base" :at +t0+)))
    (git* repo '("checkout" "-q" "-b" "topic"))
    (put-file repo "shared.txt"
              (if conflict
                  (lines "theirs" "2" "3" "4" "5" "6" "7" "8")
                  (lines "1" "2" "3" "4" "5" "6" "7" "theirs")))
    (put-file repo "theirs.txt" (lines "theirs"))
    (when conflict (remove-file repo "md.txt"))
    (let ((theirs (commit-all repo "t1" :at (+ +t0+ 100))))
      (git* repo '("checkout" "-q" "main"))
      (put-file repo "shared.txt" (lines "ours" "2" "3" "4" "5" "6" "7" "8"))
      (put-file repo "ours.txt" (lines "ours"))
      (put-file repo "md.txt" (lines "md" "edited"))
      (let ((ours (commit-all repo "o1" :at (+ +t0+ 200))))
        (list :base base :ours ours :theirs theirs)))))

(defun build-chain (repo)
  "main: c1 ← c2 ← c3 ← c4, each adding step-N.txt and rewriting log.txt;
c1 also adds .gitignore (*.log); c4 removes step-2.txt. Checked out: main
(c4). → (:c1 … :c4)."
  (put-file repo ".gitignore" (lines "*.log"))
  (let ((ids '()))
    (loop for n from 1 to 4
          do (put-file repo (format nil "step-~d.txt" n) (lines (format nil "~d" n)))
             (put-file repo "log.txt" (lines (format nil "at ~d" n)))
             (when (= n 4) (remove-file repo "step-2.txt"))
             (push (intern (format nil "C~d" n) :keyword) ids)
             (push (commit-all repo (format nil "c~d" n) :at (+ +t0+ (* 100 n))) ids))
    (nreverse ids)))

(defun sample-patch (repo from to)
  "Real `git diff --binary --full-index FROM TO` → its bytes (octets: patches
of non-UTF-8 files are not text)."
  (git* repo (list "diff" "--binary" "--full-index" "--no-color" "--no-ext-diff" from to)
        :output :octets))
