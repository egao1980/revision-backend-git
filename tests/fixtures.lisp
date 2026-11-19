(in-package #:revision-backend-git/tests)

;;; History builders. Each takes a fresh TREPO, builds a history with the real
;;; git and fixed dates, and returns a plist of names → commit ids. Dates are
;;; unix seconds; committer date = :recorded, author date = :valid.

(defparameter +t0+ 1700000000)

(defun build-linear (repo)
  "main: c1 ← c2 ← c3.
c1  at t0        author t0        a.txt
c2  at t0+100    author t0-10000  a.txt edited, b.txt added   (backdated author)
c3  at t0+200    author t0+150    b.txt removed, dir/c.txt added
Tags: v1 (lightweight) at c1, v2 (annotated, \"release two\") at c2."
  (put-file repo "a.txt" (lines "one"))
  (let ((c1 (commit-all repo "first" :at +t0+)))
    (put-file repo "a.txt" (lines "one" "two"))
    (put-file repo "b.txt" (lines "bee"))
    (let ((c2 (commit-all repo "second" :at (+ +t0+ 100) :author-at (- +t0+ 10000))))
      (remove-file repo "b.txt")
      (put-file repo "dir/c.txt" (lines "see"))
      (let ((c3 (commit-all repo (format nil "third~%~%with a body") :at (+ +t0+ 200)
                                                                    :author-at (+ +t0+ 150))))
        (git* repo (list "tag" "v1" c1))
        (git* repo (list "tag" "-a" "v2" "-m" "release two" c2)
              :env (date-env (+ +t0+ 300)))
        (list :c1 c1 :c2 c2 :c3 c3)))))

(defun build-merge (repo)
  "base ← m1 ← x (merge of f2, --no-ff) on main; base ← f1 ← f2 on feature.
Committer dates: base t0, f1 t0+100, f2 t0+200, m1 t0+300, x t0+400.
Author dates: f1 t0-5000 (old), others = committer date.
Files: base.txt; feature.txt (f1, edited in f2); main.txt (m1)."
  (put-file repo "base.txt" (lines "base"))
  (let ((base (commit-all repo "base" :at +t0+)))
    (git* repo '("checkout" "-q" "-b" "feature"))
    (put-file repo "feature.txt" (lines "f1"))
    (let ((f1 (commit-all repo "f1" :at (+ +t0+ 100) :author-at (- +t0+ 5000))))
      (put-file repo "feature.txt" (lines "f1" "f2"))
      (let ((f2 (commit-all repo "f2" :at (+ +t0+ 200))))
        (git* repo '("checkout" "-q" "main"))
        (put-file repo "main.txt" (lines "m1"))
        (let ((m1 (commit-all repo "m1" :at (+ +t0+ 300))))
          (git* repo '("merge" "-q" "--no-ff" "-m" "merge feature" "feature")
                :env (date-env (+ +t0+ 400)))
          (list :base base :f1 f1 :f2 f2 :m1 m1 :x (rev repo "HEAD")))))))

(defun build-criss-cross (repo)
  "a; b (on left) and c (on right) from a; d = merge(b, c) on left; e = merge(c, b)
on right. merge-base --all d e = {b, c}. Also an unrelated root u on orphan."
  (put-file repo "a.txt" (lines "a"))
  (let ((a (commit-all repo "a" :at +t0+)))
    (git* repo '("checkout" "-q" "-b" "left"))
    (put-file repo "b.txt" (lines "b"))
    (let ((b (commit-all repo "b" :at (+ +t0+ 100))))
      (git* repo (list "checkout" "-q" "-b" "right" a))
      (put-file repo "c.txt" (lines "c"))
      (let ((c (commit-all repo "c" :at (+ +t0+ 200))))
        (git* repo '("checkout" "-q" "left"))
        (git* repo (list "merge" "-q" "--no-ff" "-m" "d" c) :env (date-env (+ +t0+ 300)))
        (let ((d (rev repo "HEAD")))
          (git* repo '("checkout" "-q" "right"))
          (git* repo (list "merge" "-q" "--no-ff" "-m" "e" b) :env (date-env (+ +t0+ 400)))
          (let ((e (rev repo "HEAD")))
            (git* repo '("checkout" "-q" "--orphan" "other"))
            (git* repo '("rm" "-rq" "--cached" "."))
            (dolist (f '("a.txt" "b.txt" "c.txt"))
              (ignore-errors (remove-file repo f)))
            (put-file repo "u.txt" (lines "u"))
            (let ((u (commit-all repo "u" :at (+ +t0+ 500))))
              (git* repo '("checkout" "-q" "-f" "left"))
              (list :a a :b b :c c :d d :e e :u u))))))))

;;; Content history: every kind of file change git reports.
;;; core.fileMode / core.symlinks are off in this repo so `add -A` keeps the
;;; modes and symlink entries recorded in the index on every platform.

(defparameter +main-v1+ (lines "(defun main ()" "  (print :hello))" "" "(main)"))
(defparameter +main-v2+ (lines "(defun main ()" "  (print :hello-world))" "" "(main)"))
(defparameter +ten-lines+ (lines "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
(defparameter +ten-lines-edited+ (lines "1" "2" "3" "4" "5" "6" "7" "8" "9" "ten"))
(defparameter +pure+ (lines "p1" "p2" "p3" "p4" "p5"))
(defparameter +bin-v1+ (octets 0 1 2 3 255 0 10))
(defparameter +bin-v2+ (octets 0 1 2 4 255 0 10 7))
(defparameter +latin-v1+ (octets #xFF #xFE 97 98 99 10))       ; not UTF-8, no NUL
(defparameter +latin-v2+ (octets #xFF #xFE 97 98 100 10))
(defparameter +crlf-v1+ (format nil "a~c~cb~c~c" #\Return #\Newline #\Return #\Newline))
(defparameter +crlf-v2+ (format nil "a~c~cB~c~c" #\Return #\Newline #\Return #\Newline))

(defun %content-config (repo)
  (git* repo '("config" "core.fileMode" "false"))
  (git* repo '("config" "core.symlinks" "false")))

(defun %symlink (repo path target)
  "Index entry PATH → symlink to TARGET (and the plain file core.symlinks=false
expects on disk)."
  (put-file repo path target)
  (git* repo (list "add" path))
  (index-entry repo "120000" (hash-blob repo target) path))

(defun build-content (repo)
  "k1 → k2 → k3 on main.
k1: README, src/main.lisp, old/name.txt (10 lines), pure.txt, run.sh (644),
    bin/data.bin (NUL bytes), latin.txt (invalid UTF-8), crlf.txt, empty.txt,
    thing (a file), link → README (symlink).
k2: src/main.lisp edited; old/name.txt → new/name.txt with one line edited;
    pure.txt → moved.txt unchanged; run.sh +x; bin/data.bin, latin.txt,
    crlf.txt edited; empty.txt removed; docs/new.md added; README edited;
    link → src/main.lisp.
k3: thing (file) replaced by thing/inner.txt (a directory)."
  (%content-config repo)
  (put-file repo "README" (lines "hello"))
  (put-file repo "src/main.lisp" +main-v1+)
  (put-file repo "old/name.txt" +ten-lines+)
  (put-file repo "pure.txt" +pure+)
  (put-file repo "run.sh" (lines "echo run"))
  (put-file repo "bin/data.bin" +bin-v1+)
  (put-file repo "latin.txt" +latin-v1+)
  (put-file repo "crlf.txt" +crlf-v1+)
  (put-file repo "empty.txt" "")
  (put-file repo "thing" (lines "a file"))
  (git* repo '("add" "-A"))
  (%symlink repo "link" "README")
  (let ((k1 (commit-index repo "k1" :at +t0+)))
    (put-file repo "src/main.lisp" +main-v2+)
    (remove-file repo "old/name.txt")
    (put-file repo "new/name.txt" +ten-lines-edited+)
    (remove-file repo "pure.txt")
    (put-file repo "moved.txt" +pure+)
    (put-file repo "bin/data.bin" +bin-v2+)
    (put-file repo "latin.txt" +latin-v2+)
    (put-file repo "crlf.txt" +crlf-v2+)
    (remove-file repo "empty.txt")
    (put-file repo "docs/new.md" (lines "# New" "" "Doc."))
    (put-file repo "README" (lines "hello" "world"))
    (git* repo '("add" "-A"))
    (git* repo '("update-index" "--chmod=+x" "run.sh"))
    (%symlink repo "link" "src/main.lisp")
    (let ((k2 (commit-index repo "k2" :at (+ +t0+ 100))))
      (remove-file repo "thing")
      (put-file repo "thing/inner.txt" (lines "inner"))
      (let ((k3 (commit-all repo "k3" :at (+ +t0+ 200))))
        (list :k1 k1 :k2 k2 :k3 k3)))))

(defun build-ambiguous (repo)
  "Writes 600 unreachable root commits (hash-object) → (values prefix
candidate-ids): PREFIX is a 4-hex prefix shared by at least two of them, the
candidates every commit object whose id starts with PREFIX."
  (let* ((empty-tree (git repo "mktree"))
         (files (loop for i from 1 to 600
                      collect (write-octets
                               (merge-pathnames (format nil "c~d" i) (trepo-root repo))
                               (utf8 (format nil "tree ~a~%author A <a@example.invalid> 1 +0000~%committer A <a@example.invalid> 1 +0000~%~%m~d~%"
                                             empty-tree i)))))
         (ids (uiop:split-string
               (string-right-trim '(#\Newline)
                                  (git* repo '("hash-object" "-t" "commit" "-w" "--stdin-paths")
                                        :input (format nil "~{~a~%~}"
                                                       (mapcar #'uiop:native-namestring files))))
               :separator '(#\Newline)))
         (seen (make-hash-table :test #'equal))
         (prefix (loop for id in ids
                       for p = (subseq id 0 4)
                       when (gethash p seen) return p
                       do (setf (gethash p seen) t))))
    (assert prefix () "no shared 4-hex prefix among 600 commits")
    (values prefix
            (remove-duplicates
             (remove-if-not (lambda (id) (string= prefix id :end2 4)) ids)
             :test #'string=))))

(defun build-working-copy (repo)
  "HEAD c1 on main with tracked.txt, staged.txt, gone.txt and .gitignore
(ignoring *.log). Returns (:c1 id). No local changes yet."
  (put-file repo "tracked.txt" (lines "t"))
  (put-file repo "staged.txt" (lines "s"))
  (put-file repo "gone.txt" (lines "g"))
  (put-file repo ".gitignore" (lines "*.log"))
  (list :c1 (commit-all repo "c1" :at +t0+)))

(defun dirty-working-copy (repo)
  "Local changes on top of BUILD-WORKING-COPY: tracked.txt modified
(unstaged), staged.txt modified and staged, gone.txt deleted, new.txt
untracked, debug.log ignored."
  (put-file repo "tracked.txt" (lines "t" "changed"))
  (put-file repo "staged.txt" (lines "s" "staged"))
  (git* repo '("add" "staged.txt"))
  (remove-file repo "gone.txt")
  (put-file repo "new.txt" (lines "new"))
  (put-file repo "debug.log" (lines "noise")))

(defun index-digest (repo)
  "Bytes of REPO's real index file (to prove it was not touched)."
  (read-octets (merge-pathnames ".git/index" (trepo-dir repo))))
