(in-package #:revision-backend-git/tests)

;;; Parsers of -z machine output: hand corpora with awkward names, live
;;; output of real git, and a deterministic fuzz round trip.

(defparameter +h1+ "1111111111111111111111111111111111111111")
(defparameter +h2+ "2222222222222222222222222222222222222222")
(defparameter +h3+ "3333333333333333333333333333333333333333")
(defparameter +z0+ "0000000000000000000000000000000000000000")

(defun nl-name (a b)
  "A file name with a newline in it."
  (format nil "~a~%~a" a b))

;;; ---- status --porcelain=v2 -z ------------------------------------------

(deftest status-v2-hand-corpus
  (let* ((input (nul-join "# branch.oid (initial)"
                          "# branch.head main"
                          (format nil "1 .M N... 100644 100644 100644 ~a ~a tracked file.txt" +h1+ +h1+)
                          (format nil "1 A. N... 000000 100755 100755 ~a ~a ~a" +z0+ +h2+ (nl-name "line" "break.sh"))
                          (format nil "2 R. N... 100644 100644 100644 ~a ~a R100 new name.txt" +h1+ +h1+)
                          "old name.txt"
                          (format nil "2 C. N... 100644 100644 100644 ~a ~a C075 copy.txt" +h1+ +h2+)
                          "orig.txt"
                          (format nil "u UU N... 100644 100644 100644 100644 ~a ~a ~a conflict.txt" +h1+ +h2+ +h3+)
                          "? untracked dir/x y.txt"
                          "? café ✓.txt"
                          "! ignored.log"
                          (format nil "1 .M SC.. 160000 160000 160000 ~a ~a vendor/lib" +h1+ +h1+)))
         (entries (git:parse-status-porcelain-v2 input)))
    (ok (= 9 (length entries)) "headers skipped, one plist per entry")
    (when (= 9 (length entries))
      (destructuring-bind (e1 e2 e3 e4 e5 e6 e7 e8 e9) entries
        (ok (plist-matches-p (list :kind :changed :xy ".M" :submodule "N..."
                                   :mode-head #o100644 :mode-index #o100644 :mode-worktree #o100644
                                   :id-head +h1+ :id-index +h1+ :path "tracked file.txt")
                             e1))
        (ok (plist-matches-p (list :kind :changed :xy "A." :mode-head 0 :mode-index #o100755
                                   :id-head +z0+ :id-index +h2+ :path (nl-name "line" "break.sh"))
                             e2)
            "newline inside a name")
        (ok (plist-matches-p (list :kind :renamed :xy "R." :score 100 :path "new name.txt"
                                   :orig-path "old name.txt")
                             e3))
        (ok (plist-matches-p (list :kind :copied :xy "C." :score 75 :path "copy.txt"
                                   :orig-path "orig.txt" :id-index +h2+)
                             e4))
        (ok (plist-matches-p (list :kind :unmerged :xy "UU" :submodule "N..."
                                   :modes (list #o100644 #o100644 #o100644 #o100644)
                                   :ids (list +h1+ +h2+ +h3+) :path "conflict.txt")
                             e5))
        (ok (plist-matches-p '(:kind :untracked :path "untracked dir/x y.txt") e6))
        (ok (plist-matches-p '(:kind :untracked :path "café ✓.txt") e7))
        (ok (plist-matches-p '(:kind :ignored :path "ignored.log") e8))
        (ok (plist-matches-p (list :kind :changed :submodule "SC.." :mode-head #o160000
                                   :path "vendor/lib")
                             e9))))))

(deftest status-v2-empty-and-malformed
  (ok (null (git:parse-status-porcelain-v2 "")))
  (ok (null (git:parse-status-porcelain-v2 (nul-join "# branch.oid abc" "# branch.head main"))))
  (ok (signals (git:parse-status-porcelain-v2 (nul-join "Z what is this")) 'rev:revision-error))
  (ok (signals (git:parse-status-porcelain-v2 (nul-join "1 .M N... 100644")) 'rev:revision-error)
      "truncated record"))

(deftest status-v2-live
  (with-test-repo (r)
    (build-working-copy r)
    (put-file r "rename-me.txt" (lines "r"))
    (commit-all r "c2" :at (+ +t0+ 50))
    (dirty-working-copy r)
    (git* r '("mv" "rename-me.txt" "renamed.txt"))
    (let* ((out (git* r '("status" "--porcelain=v2" "-z" "--untracked-files=all" "--ignored")))
           (entries (git:parse-status-porcelain-v2 out)))
      (flet ((entry (path) (find path entries :key (lambda (e) (getf e :path)) :test #'equal)))
        (ok (= 6 (length entries)))
        (ok (plist-matches-p (list :kind :changed :xy ".M"
                                   :id-head (git r "rev-parse" "HEAD:tracked.txt"))
                             (entry "tracked.txt")))
        (ok (plist-matches-p '(:kind :changed :xy "M.") (entry "staged.txt")))
        (ok (plist-matches-p '(:kind :changed :xy ".D") (entry "gone.txt")))
        (ok (plist-matches-p '(:kind :renamed :xy "R." :score 100 :orig-path "rename-me.txt")
                             (entry "renamed.txt")))
        (ok (plist-matches-p '(:kind :untracked) (entry "new.txt")))
        (ok (plist-matches-p '(:kind :ignored) (entry "debug.log")))))))

;;; ---- name-status -z ------------------------------------------------------

(deftest name-status-hand-corpus
  (let ((entries (git:parse-name-status
                  (nul-join "A" "new file.txt"
                            "D" (nl-name "gone" "away")
                            "M" "dir/mod.lisp"
                            "T" "link"
                            "U" "conflict"
                            "R087" "old name" "new name"
                            "C100" "src" "src copy"
                            "M100" "scored"))))
    (ok (= 8 (length entries)))
    (when (= 8 (length entries))
      (ok (every #'plist-matches-p
                 (list '(:status :add :path "new file.txt")
                       (list :status :remove :path (nl-name "gone" "away"))
                       '(:status :modify :path "dir/mod.lisp")
                       '(:status :type-change :path "link")
                       '(:status :unmerged :path "conflict")
                       '(:status :move :score 87 :from "old name" :path "new name")
                       '(:status :copy :score 100 :from "src" :path "src copy")
                       '(:status :modify :score 100 :path "scored"))
                 entries)))
    (ok (null (git:parse-name-status "")))
    (ok (signals (git:parse-name-status (nul-join "Q" "x")) 'rev:revision-error))
    (ok (signals (git:parse-name-status (nul-join "R100" "only-one-path")) 'rev:revision-error))))

(deftest name-status-live
  (with-test-repo (r)
    (let* ((k (build-content r))
           (entries (git:parse-name-status
                     (git* r (list "diff-tree" "-r" "-z" "-M" "--name-status" "--no-commit-id"
                                   (getf k :k1) (getf k :k2)))))
           (summary (mapcar (lambda (e) (list (getf e :status) (getf e :from) (getf e :path)))
                            entries)))
      (ok (set= '((:modify nil "README") (:modify nil "bin/data.bin") (:modify nil "crlf.txt")
                  (:add nil "docs/new.md") (:remove nil "empty.txt") (:modify nil "latin.txt")
                  (:modify nil "link") (:move "pure.txt" "moved.txt")
                  (:move "old/name.txt" "new/name.txt") (:modify nil "run.sh")
                  (:modify nil "src/main.lisp"))
                summary))
      (let ((pure (find "moved.txt" entries :key (lambda (e) (getf e :path)) :test #'equal))
            (edit (find "new/name.txt" entries :key (lambda (e) (getf e :path)) :test #'equal)))
        (ok (eql 100 (getf pure :score)))
        (ok (and (integerp (getf edit :score)) (<= 50 (getf edit :score) 99)))))))

;;; ---- ls-tree -z -----------------------------------------------------------

(deftest ls-tree-hand-corpus
  (let ((entries (git:parse-ls-tree
                  (nul-join (format nil "100644 blob ~a      12~cREADME" +h1+ #\Tab)
                            (format nil "100755 blob ~a       9~crun me.sh" +h2+ #\Tab)
                            (format nil "120000 blob ~a      13~c~a" +h3+ #\Tab (nl-name "li" "nk"))
                            (format nil "040000 tree ~a       -~csrc" +h1+ #\Tab)
                            (format nil "160000 commit ~a       -~cvendor/lib" +h2+ #\Tab)
                            (format nil "100644 blob ~a~cno-size.txt" +h3+ #\Tab)))))
    (ok (= 6 (length entries)))
    (when (= 6 (length entries))
      (ok (every #'plist-matches-p
                 (list (list :mode #o100644 :type :blob :id +h1+ :size 12 :path "README")
                       (list :mode #o100755 :type :blob :id +h2+ :size 9 :path "run me.sh")
                       (list :mode #o120000 :type :blob :id +h3+ :size 13 :path (nl-name "li" "nk"))
                       (list :mode #o40000 :type :tree :id +h1+ :size nil :path "src")
                       (list :mode #o160000 :type :commit :id +h2+ :size nil :path "vendor/lib")
                       (list :mode #o100644 :type :blob :id +h3+ :size nil :path "no-size.txt"))
                 entries)))
    (ok (null (git:parse-ls-tree "")))
    (ok (signals (git:parse-ls-tree (nul-join "100644 blob")) 'rev:revision-error))))

(deftest ls-tree-live
  (with-test-repo (r)
    (let* ((k2 (getf (build-content r) :k2))
           (entries (git:parse-ls-tree (git* r (list "ls-tree" "-z" "-l" "-r" k2))))
           (top (git:parse-ls-tree (git* r (list "ls-tree" "-z" "-l" k2)))))
      (flet ((entry (list path) (find path list :key (lambda (e) (getf e :path)) :test #'equal)))
        (ok (= 11 (length entries)))
        (ok (plist-matches-p (list :mode #o120000 :type :blob :size 13
                                   :id (git r "rev-parse" (format nil "~a:link" k2)))
                             (entry entries "link")))
        (ok (plist-matches-p '(:mode #o100755 :size 9) (entry entries "run.sh")))
        (ok (plist-matches-p '(:mode #o100644 :size 8) (entry entries "bin/data.bin")))
        (ok (plist-matches-p '(:type :tree :size nil) (entry top "src")))
        (ok (null (entry top "src/main.lisp")) "not recursive without -r")))))

;;; ---- fuzz: render with git's grammar, parse back ------------------------

(defparameter +fuzz-alphabet+
  (concatenate 'string "abcXYZ019 ._-+=@#$%&()[]{}',;~"
               (string #\Tab) (string #\Newline) (string #\Return)
               "\"\\" "éüλ中"))

(defun make-lcg (seed)
  (let ((x seed))
    (lambda (n)
      (setf x (mod (+ (* x 6364136223846793005) 1442695040888963407) (expt 2 64)))
      (mod (ash x -33) n))))

(defun fuzz-name (rand)
  (let ((segments (1+ (funcall rand 3))))
    (format nil "~{~a~^/~}"
            (loop repeat segments
                  collect (coerce (loop repeat (1+ (funcall rand 8))
                                        collect (char +fuzz-alphabet+
                                                      (funcall rand (length +fuzz-alphabet+))))
                                  'string)))))

(defun fuzz-id (rand)
  (coerce (loop repeat 40 collect (char "0123456789abcdef" (funcall rand 16))) 'string))

(deftest-parametrize parsers-fuzz-round-trip
    ((seed) :ids ("s1" "s2" "s3" "s4" "s5")
     (1) (2) (3) (4) (5))
  (let ((rand (make-lcg seed))
        (status-records '()) (status-expected '())
        (ns-records '()) (ns-expected '())
        (ls-records '()) (ls-expected '()))
    (dotimes (i 20)
      (let ((path (fuzz-name rand)) (orig (fuzz-name rand))
            (h (fuzz-id rand)) (i2 (fuzz-id rand)))
        ;; status
        (ecase (funcall rand 4)
          (0 (push (format nil "1 .M N... 100644 100644 100644 ~a ~a ~a" h i2 path) status-records)
             (push (list :kind :changed :xy ".M" :id-head h :id-index i2 :path path) status-expected))
          (1 (push (format nil "2 R. N... 100644 100644 100644 ~a ~a R~d ~a" h h (+ 50 (funcall rand 51)) path)
                   status-records)
             (push orig status-records)
             (push (list :kind :renamed :path path :orig-path orig) status-expected))
          (2 (push (format nil "? ~a" path) status-records)
             (push (list :kind :untracked :path path) status-expected))
          (3 (push (format nil "! ~a" path) status-records)
             (push (list :kind :ignored :path path) status-expected)))
        ;; name-status
        (if (zerop (funcall rand 2))
            (progn (push "M" ns-records) (push path ns-records)
                   (push (list :status :modify :path path) ns-expected))
            (let ((score (+ 50 (funcall rand 51))))
              (push (format nil "R~3,'0d" score) ns-records) (push orig ns-records) (push path ns-records)
              (push (list :status :move :score score :from orig :path path) ns-expected)))
        ;; ls-tree
        (let ((size (funcall rand 100000)))
          (push (format nil "100644 blob ~a ~7d~c~a" h size #\Tab path) ls-records)
          (push (list :mode #o100644 :type :blob :id h :size size :path path) ls-expected))))
    (flet ((check (name parser records expected)
             (let ((parsed (funcall parser (apply #'nul-join (reverse records))))
                   (expected (reverse expected)))
               (ok (and (= (length parsed) (length expected))
                        (every #'plist-matches-p expected parsed))
                   name))))
      (check "status v2" #'git:parse-status-porcelain-v2 status-records status-expected)
      (check "name-status" #'git:parse-name-status ns-records ns-expected)
      (check "ls-tree" #'git:parse-ls-tree ls-records ls-expected))))
