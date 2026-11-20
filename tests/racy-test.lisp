(in-package #:revision-backend-git/tests)

;;; Racy git (#5): the stat index trusts stat data only under git's racy-git
;;; rule, so a same-size rewrite in the same second (inode kept) is drift.

(defun rewrite-in-place (repo relative content)
  "Overwrite RELATIVE's bytes with CONTENT (same length) without truncating or
replacing the file: same inode, same size."
  (with-open-file (out (repo-file repo relative) :direction :output
                                                 :element-type '(unsigned-byte 8)
                                                 :if-exists :overwrite)
    (write-sequence (utf8 content) out)))

(defun at-a-fresh-second ()
  "Return right after the wall clock's second changes, so the next steps
usually fit in one second."
  (let ((now (get-universal-time)))
    (loop while (= now (get-universal-time)) do (sleep 0.005))))

(defun drift-paths (wc store)
  (let ((d (rev:target-drift wc store)))
    (and d (sort (remove-duplicates (mapcar (lambda (c) (first (diff:change-path c))) (diff:diff-changes d)) :test #'equal)
                 #'string<))))

(deftest same-second-same-size-rewrite-is-drift
  (with-test-repo (r)
    (put-file r "a.txt" (lines "aaaa"))
    (put-file r "b.txt" (lines "bbbb"))
    (commit-all r "c1")
    (with-store (s r)
      (let ((wc (git:make-git-working-copy s))
            (index (index-hash r)))
        (dotimes (i 3)
          (at-a-fresh-second)
          (ok (null (rev:target-drift wc s)) "clean")
          (rewrite-in-place r "a.txt" (lines "zzzz"))
          (ok (equal '("a.txt") (drift-paths wc s)) "the in-place rewrite is drift")
          (rewrite-in-place r "a.txt" (lines "aaaa"))
          (ok (null (rev:target-drift wc s)) "and the rewrite back is clean again"))
        (ok (equal index (index-hash r)) "real index untouched")))))

(deftest same-second-rewrite-after-a-write-is-drift
  (with-test-repo (r)
    (put-file r "a.txt" (lines "one!"))
    (put-file r "keep.txt" (lines "unchanged"))
    (let ((c1 (commit-all r "c1")))
      (put-file r "a.txt" (lines "two!"))
      (let ((c2 (commit-all r "c2" :at (+ +t0+ 10))))
        (with-store (s r)
          (let ((wc (git:make-git-working-copy s :stamp-ref "refs/checkpoints/racy"))
                (keep-date (file-write-date (repo-file r "keep.txt"))))
            (rev:target-stamp wc (list c1))
            (dotimes (i 3)
              (at-a-fresh-second)
              (setf (rev:target-state wc) (rev:revision-state s c1))
              (ok (null (rev:target-drift wc s)) "the write leaves no drift")
              (rewrite-in-place r "a.txt" (lines "two!"))
              (ok (equal '("a.txt") (drift-paths wc s)) "a rewrite right after the write is drift"))
            (at-a-fresh-second)
            (setf (rev:target-state wc) (rev:revision-state s c2))
            (setf (rev:target-state wc) (rev:revision-state s c1))
            (ok (eql keep-date (file-write-date (repo-file r "keep.txt")))
                "files equal to the goal are not rewritten")))))))
