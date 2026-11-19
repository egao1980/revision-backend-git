(in-package #:revision-backend-git/tests)

;;; The revision-protocol law rows that apply to a git store.
;;;   Law 1: the graph is acyclic (git ids are content hashes; caller ids are
;;;          refused, see commit-refusals-are-atomic); revisions never change;
;;;          tags never move (tags-never-move); ref CAS is linearizable — here
;;;          also across threads sharing one store.
;;;   Law 2: as-of is monotonic along its line, both axes.
;;;   Law 3: apply / inverse / self-diff (state-diff-laws in content-test);
;;;          revision deltas compose associatively.
;;; Laws 4–6 (three-way, plans, projections) are V1.

(defun snapshot (rv)
  (list (rev:revision-id rv) (rev:revision-parent-ids rv) (rev:revision-message rv)
        (rev:revision-content rv) (rev:revision-provenance rv)
        (unix (rev:revision-recorded-at rv)) (unix (rev:revision-valid-at rv))))

(deftest law-1-revisions-never-change
  (with-test-repo (r)
    (let ((k (build-merge r)))
      (with-store (s r)
        (let ((before (snapshot (rev:resolve-revision s (getf k :m1)))))
          (rev:commit-revision s "HEAD^{tree}" :ref "main" :message "later")
          (rev:move-ref s "main" (getf k :base))
          (rev:tag-revision s "keep" (getf k :m1) :annotation "kept")
          (rev:delete-ref s "feature")
          (ok (equal before (snapshot (rev:resolve-revision s (getf k :m1))))))))))

(deftest law-1-returned-lists-are-not-shared
  (with-test-repo (r)
    (let ((k (build-merge r)))
      (with-store (s r)
        (let* ((rv (rev:resolve-revision s (getf k :x)))
               (parents (rev:revision-parent-ids rv))
               (prov (rev:revision-provenance rv)))
          (setf (first parents) "mallory"
                (second prov) "mallory")
          (ok (equal (list (getf k :m1) (getf k :f2)) (rev:revision-parent-ids rv)))
          (ok (equal "Test User" (getf (rev:revision-provenance rv) :author-name))))))))

(deftest-parametrize law-1-ref-cas-linearizable
    ((threads per-thread) :ids ("2x10" "4x5")
     (2 10)
     (4 5))
  ;; Workers commit onto main with the default parent (the tip), retrying on
  ;; REF-CONFLICT. Linearizable CAS ⇒ one line of THREADS × PER-THREAD + 1
  ;; revisions, a single head, and git agrees.
  (with-test-repo (r)
    (build-working-copy r)
    (with-store (s r)
      (let ((failures '())
            (lock (bt2:make-lock :name "failures")))
        (mapc #'bt2:join-thread
              (loop for i below threads
                    collect (let ((i i))
                              (bt2:make-thread
                               (lambda ()
                                 (handler-case
                                     (handler-bind ((rev:ref-conflict
                                                      (lambda (c) (declare (ignore c))
                                                        (invoke-restart 'rev:retry))))
                                       (dotimes (n per-thread)
                                         (rev:commit-revision
                                          s (diff:make-tree "n.txt"
                                                            (diff:make-tree-file (format nil "~d ~d~%" i n)))
                                          :ref "main" :message (format nil "w~d n~d" i n))))
                                   (error (e)
                                     (bt2:with-lock-held (lock)
                                       (push (princ-to-string e) failures)))))
                               :name (format nil "committer ~d" i)))))
        (ok (null failures) (format nil "worker errors: ~{~a~^; ~}" (last failures 1)))
        (when (null failures)
          (let ((n (1+ (* threads per-thread))))
            (ok (= n (length (rev:walk-revisions s (rev:rev-ref "main") :order :first-parent))))
            (ok (= n (parse-integer (git r "rev-list" "--count" "--first-parent" "main"))))
            (ok (= n (parse-integer (git r "rev-list" "--count" "main"))) "no merges, no lost commits")
            (ok (= 1 (length (rev:heads s))))))))))

(deftest-parametrize law-2-as-of-monotonic
    ((axis t1 t2) :ids ("rec-0-100" "rec-50-250" "rec-100-100" "val-0-150" "val-minus-0" "val-149-150")
     (:recorded 0 100) (:recorded 50 250) (:recorded 100 100)
     (:valid 0 150) (:valid -10000 0) (:valid 149 150))
  (with-test-repo (r)
    (build-linear r)
    (with-store (s r)
      (flet ((at (offset)
               (rev:resolve-revision s (rev:rev-as-of (instant (+ +t0+ offset)) :on "main" :axis axis))))
        (ok (rev:ancestor-p s (at t1) (at t2)) "earlier time → ancestor-or-self")))))

(deftest law-3-revision-deltas-compose-associatively
  (with-test-repo (r)
    (let ((k (build-content r)))
      (with-store (s r)
        (let ((d1 (rev:revision-delta s (getf k :k1)))
              (d2 (rev:revision-delta s (getf k :k2)))
              (d3 (rev:revision-delta s (getf k :k3)))
              (empty (diff:make-tree))
              (s3 (rev:revision-state s (getf k :k3))))
          (ok (state= s3 (diff:apply-diff empty (diff:compose-diffs (diff:compose-diffs d1 d2) d3))))
          (ok (state= s3 (diff:apply-diff empty (diff:compose-diffs d1 (diff:compose-diffs d2 d3))))))))))
