(in-package #:revision-backend-git/tests)

;;; Content: states as tree values (text / binary / modes / symlinks /
;;; gitlinks), deltas, and STATE-DIFF at every level against git.

(defun file-at (tree path)
  (cdr (assoc path (tree-files tree) :test #'string=)))

(defun mode-of (file)
  (or (diff:tree-file-mode file) #o100644))

(defun names (path-string)
  (uiop:split-string path-string :separator '(#\/)))

(defparameter +k1-files+
  ;; path type content mode
  `(("README" :text ,(lines "hello") #o100644)
    ("bin/data.bin" :binary ,+bin-v1+ #o100644)
    ("crlf.txt" :text ,+crlf-v1+ #o100644)
    ("empty.txt" :text "" #o100644)
    ("latin.txt" :binary ,+latin-v1+ #o100644)
    ("link" :text "README" #o120000)
    ("old/name.txt" :text ,+ten-lines+ #o100644)
    ("pure.txt" :text ,+pure+ #o100644)
    ("run.sh" :text ,(lines "echo run") #o100644)
    ("src/main.lisp" :text ,+main-v1+ #o100644)
    ("thing" :text ,(lines "a file") #o100644)))

(deftest content-primary-and-engine
  (with-test-repo (r)
    (with-store (s r)
      (ok (eq :state (rev:content-primary s)))
      (ok (typep (rev:content-engine s) 'diff:tree-diff-engine)))))

(deftest revision-state-reads-every-kind-of-file
  (with-test-repo (r)
    (let* ((k (build-content r)))
      (with-store (s r)
        (let ((state (rev:revision-state s (getf k :k1))))
          (ok (diff:tree-dir-p state))
          (ok (equal (mapcar #'first +k1-files+) (mapcar #'car (tree-files state))) "every file, sorted")
          (dolist (row +k1-files+)
            (destructuring-bind (path type content mode) row
              (let ((f (file-at state path)))
                (ok (and f
                         (eq type (diff:tree-file-type f))
                         (equalp content (diff:tree-file-content f))
                         (= mode (mode-of f)))
                    (format nil "~a: ~(~a~) ~o" path type mode))))))
        (let ((state (rev:revision-state s (getf k :k2))))
          (ok (= #o100755 (mode-of (file-at state "run.sh"))) "executable bit")
          (ok (equal "src/main.lisp" (diff:tree-file-content (file-at state "link"))) "symlink target")
          (ok (equalp +bin-v2+ (diff:tree-file-content (file-at state "bin/data.bin"))))
          (ok (null (file-at state "empty.txt"))))
        (let ((state (rev:revision-state s (getf k :k3))))
          (ok (diff:tree-dir-p (diff:tree-get state '("thing"))) "file replaced by a directory")
          (ok (equal (lines "inner") (diff:tree-file-content (file-at state "thing/inner.txt")))))
        (ok (state= (diff:make-tree)
                    (rev:revision-state s (rev:commit-revision s (diff:make-tree) :message "empty")))
            "the empty tree is the empty state")))))

(deftest revision-state-gitlink
  (with-test-repo (r)
    (put-file r "x.txt" (lines "x"))
    (git* r '("add" "x.txt"))
    (index-entry r "160000" +h1+ "vendor/lib")
    (let ((c (commit-index r "with a submodule entry")))
      (with-store (s r)
        (let ((f (file-at (rev:revision-state s c) "vendor/lib")))
          (ok (and f (eq :gitlink (diff:tree-file-type f))))
          (ok (and f (equal +h1+ (diff:tree-file-content f))))
          (ok (and f (= #o160000 (mode-of f)))))))))

(deftest revision-delta-against-the-first-parent
  (with-test-repo (r)
    (let ((k (build-content r)))
      (with-store (s r)
        (let ((d2 (rev:revision-delta s (getf k :k2)))
              (d1 (rev:revision-delta s (getf k :k1))))
          (ok (typep d2 'diff:diff))
          (ok (state= (rev:revision-state s (getf k :k2))
                      (diff:apply-diff (rev:revision-state s (getf k :k1)) d2)))
          (ok (state= (rev:revision-state s (getf k :k1)) (diff:apply-diff (diff:make-tree) d1))
              "a root's delta is from the empty tree")
          (ok (rev:delta-inverse d2) "git deltas always invert (no opaque changes)"))))))

(defun git-name-status-summary (repo a b)
  "Real git (no -z; fixture names are plain): set of (kind from-names names)."
  (loop for line in (git-lines repo "diff-tree" "-r" "-M" "--name-status" a b)
        for fields = (uiop:split-string line :separator '(#\Tab))
        for letter = (char (first fields) 0)
        collect (ecase letter
                  (#\A (list :add nil (names (second fields))))
                  (#\D (list :remove nil (names (second fields))))
                  ((#\M #\T) (list :modify nil (names (second fields))))
                  (#\R (list :move (names (second fields)) (names (third fields)))))))

(defun all-changes (d)
  (labels ((walk (c) (cons c (mapcan #'walk (diff:change-children c)))))
    (mapcan #'walk (diff:diff-changes d))))

(deftest state-diff-summary-matches-diff-tree
  (with-test-repo (r)
    (let ((k (build-content r)))
      (with-store (s r)
        (dolist (pair (list (list :k1 :k2) (list :k2 :k3) (list :k1 :k3)))
          (let* ((a (getf k (first pair))) (b (getf k (second pair)))
                 (d (rev:state-diff s a b :level :summary))
                 (changes (diff:diff-changes d)))
            (ok (eq :summary (diff:diff-level d)))
            (ok (set= (git-name-status-summary r a b)
                      (mapcar (lambda (c) (list (diff:change-kind c) (diff:change-from c) (diff:change-path c)))
                              changes))
                (format nil "~(~a → ~a~): kinds and paths as git" (first pair) (second pair)))
            (ok (every (lambda (c) (null (diff:change-children c))) changes) "no children")
            (ok (notany (lambda (c) (or (diff:change-before-loaded-p c) (diff:change-after-loaded-p c)))
                        changes)
                "file nodes are lazy")
            (ok (every (lambda (c) (member (format nil "tree:/~{~a~^/~}" (diff:change-path c))
                                           (diff:change-footprint c) :test #'string=))
                       changes)
                "tree footprints")))))))

(deftest state-diff-changes-and-content-shapes
  (with-test-repo (r)
    (let ((k (build-content r)))
      (with-store (s r)
        (let* ((d (rev:state-diff s (getf k :k1) (getf k :k2)))
               (all (all-changes d)))
          (ok (eq :changes (diff:diff-level d)) "default level")
          (ok (typep (diff:diff-engine d) 'diff:tree-diff-engine))
          (ok (notany (lambda (c) (typep c 'diff:opaque-change)) all) "no opaque changes")
          (ok (some (lambda (c) (and (equal '("src" "main.lisp") (subseq (diff:change-path c) 0
                                                                       (min 2 (length (diff:change-path c)))))
                                     (> (length (diff:change-path c)) 2)))
                    all)
              "text edits are hunks nested under the file")
          (let ((bin (find '("bin" "data.bin") all :key #'diff:change-path :test #'equal)))
            (ok (and bin (eq :modify (diff:change-kind bin))) "binary: a whole-file modify")
            (when bin
              (ok (equalp +bin-v1+ (diff:tree-file-content (diff:change-before bin))))
              (ok (equalp +bin-v2+ (diff:tree-file-content (diff:change-after bin))))))
          (let ((latin (find '("latin.txt") all :key #'diff:change-path :test #'equal)))
            (ok (and latin (eq :binary (diff:tree-file-type (diff:change-after latin))))
                "invalid UTF-8 without NUL is binary too"))
          (let ((mv (find '("new" "name.txt") all :key #'diff:change-path :test #'equal)))
            (ok (and mv (eq :move (diff:change-kind mv)) (equal '("old" "name.txt") (diff:change-from mv)))
                "rename with an edit")))
        (let ((d (rev:state-diff s (getf k :k1) (getf k :k2) :level :content)))
          (ok (eq :content (diff:diff-level d)))
          (let ((main (find '("src" "main.lisp") (diff:diff-changes d) :key #'diff:change-path :test #'equal)))
            (ok (and main (diff:change-children main)) "file-level change with hunk children")))))))

(deftest-parametrize state-diff-laws
    ((from to level) :ids ("k1-k2-summary" "k1-k2-changes" "k1-k2-content"
                           "k2-k3-changes" "k1-k3-changes" "k3-k1-changes" "k3-k1-summary")
     (:k1 :k2 :summary) (:k1 :k2 :changes) (:k1 :k2 :content)
     (:k2 :k3 :changes) (:k1 :k3 :changes) (:k3 :k1 :changes) (:k3 :k1 :summary))
  (with-test-repo (r)
    (let ((k (build-content r)))
      (with-store (s r)
        (let* ((a (getf k from)) (b (getf k to))
               (sa (rev:revision-state s a)) (sb (rev:revision-state s b))
               (d (rev:state-diff s a b :level level)))
          (ok (state= sb (diff:apply-diff sa d)) "apply(state a, diff(a, b)) = state b")
          (ok (state= sa (diff:apply-diff sb (diff:invert-diff d))) "the inverse goes back")
          (ok (diff:diff-empty-p (rev:state-diff s a a :level level)) "diff(a, a) is empty"))))))

(deftest state-diff-footprint
  (with-test-repo (r)
    (let ((k (build-content r)))
      (with-store (s r)
        (let ((d (rev:state-diff s (getf k :k1) (getf k :k2) :level :summary :footprint '("tree:/src"))))
          (ok (equal '(("src" "main.lisp")) (mapcar #'diff:change-path (diff:diff-changes d)))))
        (let ((d (rev:state-diff s (getf k :k1) (getf k :k2) :footprint '("tree:/new" "tree:/README"))))
          (ok (set= '(("README") ("new" "name.txt"))
                    (remove-duplicates
                     (mapcar (lambda (c) (subseq (diff:change-path c) 0
                                                 (position-if-not #'stringp (diff:change-path c))))
                             (diff:diff-changes d))
                     :test #'equal))))))))
