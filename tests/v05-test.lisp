(in-package #:revision-backend-git/tests)

;;; 0.5.0: full ref names (#8); revision-protocol 0.4.0 (#2): :EXPECTED without
;;; :REF signals, REPLACE-ANNOTATION on tags; the listener read under its lock.

(defparameter +plan-ref+ "refs/cl-stack-code/plans/p1")

(deftest full-ref-names-are-used-as-given
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (testing "move-ref on a custom namespace"
          (let ((ref (rev:move-ref s +plan-ref+ (getf k :c1) :expected nil)))
            (ok (equal +plan-ref+ (rev:ref-name ref)))
            (ok (eq :movable (rev:ref-kind ref))))
          (ok (equal (getf k :c1) (ref-id r +plan-ref+)) "the ref as given")
          (ok (null (ref-id r (concatenate 'string "refs/heads/" +plan-ref+)))
              "no refs/heads/ prefix"))
        (testing "CAS on the custom namespace"
          (ok (signals (rev:move-ref s +plan-ref+ (getf k :c2) :expected nil) 'rev:ref-conflict)
              "expected NIL: must be absent")
          (let ((c (capture-condition (rev:ref-conflict)
                     (rev:move-ref s +plan-ref+ (getf k :c2) :expected (getf k :c3)))))
            (ok (typep c 'rev:ref-conflict))
            (when c (ok (equal (getf k :c1) (rev:ref-conflict-actual c)))))
          (rev:move-ref s +plan-ref+ (getf k :c2) :expected (getf k :c1))
          (ok (equal (getf k :c2) (ref-id r +plan-ref+)))
          (git* r (list "update-ref" +plan-ref+ (getf k :c3)))
          (ok (signals (rev:move-ref s +plan-ref+ (getf k :c1) :expected (getf k :c2))
                       'rev:ref-conflict)
              "against another git process"))
        (testing "commit-revision :ref on a full name"
          (let ((new (rev:commit-revision s "HEAD^{tree}" :ref +plan-ref+
                                                          :expected (getf k :c3) :message "p")))
            (ok (equal (rev:revision-id new) (ref-id r +plan-ref+)))
            (ok (equal (list (getf k :c3)) (rev:revision-parent-ids new))
                "parent defaults to the full ref's tip")))
        (testing "resolve, find-ref and list-refs"
          (ok (equal (ref-id r +plan-ref+) (rev:revision-id (rev:resolve-revision s (rev:rev-ref +plan-ref+)))))
          (let ((ref (rev:find-ref s +plan-ref+)))
            (ok (and ref (equal +plan-ref+ (rev:ref-name ref)))))
          (ok (equal (list +plan-ref+)
                     (mapcar #'rev:ref-name (rev:list-refs s :prefix "refs/cl-stack-code/"))))
          (ok (equal '("refs/tags/v1" "refs/tags/v2")
                     (mapcar #'rev:ref-name (rev:list-refs s :prefix "refs/tags/"))))
          (ok (equal '("main" "v1" "v2") (mapcar #'rev:ref-name (rev:list-refs s)))
              "without a refs/ prefix: short names, heads and tags only"))
        (testing "refs/heads/ given in full is the branch"
          (rev:move-ref s "refs/heads/topic" (getf k :c1))
          (ok (equal (getf k :c1) (ref-id r "refs/heads/topic")))
          (ok (equal (getf k :c1) (rev:ref-target (rev:find-ref s "topic")))))
        (testing "delete-ref on a full name"
          (ok (signals (rev:delete-ref s +plan-ref+ :expected (getf k :c1)) 'rev:ref-conflict))
          (ok (eq t (rev:delete-ref s +plan-ref+)))
          (ok (null (ref-id r +plan-ref+))))
        (testing "short names keep refs/heads/"
          (rev:move-ref s "plans/p2" (getf k :c2))
          (ok (equal (getf k :c2) (ref-id r "refs/heads/plans/p2"))))))))

(deftest full-tag-names
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (testing "tag-revision with refs/tags/"
          (let ((tag (rev:tag-revision s "refs/tags/v3" (getf k :c3))))
            (ok (eq :tag (rev:ref-kind tag)))
            (ok (equal (getf k :c3) (ref-id r "refs/tags/v3")))
            (ok (null (ref-id r "refs/tags/refs/tags/v3"))))
          (rev:tag-revision s "refs/tags/v4" (getf k :c3) :annotation "four")
          (ok (equal "tag" (git r "cat-file" "-t" "refs/tags/v4")))
          (ok (equal "four" (rev:ref-annotation (rev:find-ref s "v4"))))
          (ok (equal "four" (rev:ref-annotation (rev:find-ref s "refs/tags/v4")))))
        (testing "a full tag name is immutable"
          (ok (signals (rev:move-ref s "refs/tags/v1" (getf k :c3)) 'rev:tag-immutable))
          (ok (signals (rev:delete-ref s "refs/tags/v1") 'rev:tag-immutable))
          (ok (signals (rev:commit-revision s "HEAD^{tree}" :ref "refs/tags/v1") 'rev:tag-immutable))
          (ok (signals (rev:tag-revision s "refs/tags/v1" (getf k :c3)) 'rev:tag-immutable))
          (ok (equal (getf k :c1) (ref-id r "refs/tags/v1"))))
        (testing "tag creation is a CAS"
          (git* r (list "update-ref" "refs/tags/v5" (getf k :c2)))
          (ok (signals (rev:tag-revision s "refs/tags/v5" (getf k :c3)) 'rev:tag-immutable)))
        (testing "a tag outside refs/tags/ is refused"
          (ok (signals (rev:tag-revision s "refs/custom/t" (getf k :c3)) 'rev:revision-error))
          (ok (null (ref-id r "refs/custom/t"))))))))

(deftest expected-without-ref-signals
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (let ((before (git r "rev-list" "--all" "--count")))
          (ok (signals (rev:commit-revision s "HEAD^{tree}" :expected (getf k :c3))
                       'rev:revision-error))
          (ok (signals (rev:commit-revision s "HEAD^{tree}" :expected nil) 'rev:revision-error))
          (ok (equal before (git r "rev-list" "--all" "--count")) "nothing committed"))))))

(defun %replacing (c)
  (declare (ignore c))
  (invoke-restart 'rev:replace-annotation))

(deftest replace-annotation
  (with-test-repo (r)
    (let ((k (build-linear r))
          (events '()))
      (with-store (s r)
        (ok (eq t (rev:tag-rewrite-p s)))
        (setf (rev:ref-listener s) (lambda (e) (push e events)))
        (testing "a different annotation stays immutable"
          (ok (signals (rev:tag-revision s "v2" (getf k :c2) :annotation "other") 'rev:tag-immutable))
          (ok (equal "release two" (rev:ref-annotation (rev:find-ref s "v2")))))
        (testing "offered only for the same target"
          (ok (member 'rev:replace-annotation
                      (restart-names-in 'rev:tag-immutable
                                        (lambda () (rev:tag-revision s "v2" (getf k :c2) :annotation "x")))))
          (ok (not (member 'rev:replace-annotation
                           (restart-names-in 'rev:tag-immutable
                                             (lambda () (rev:tag-revision s "v2" (getf k :c3) :annotation "x"))))))
          (ok (not (member 'rev:replace-annotation
                           (restart-names-in 'rev:tag-immutable
                                             (lambda () (rev:move-ref s "v2" (getf k :c2))))))))
        (testing "annotated → annotated"
          (let ((old (ref-id r "refs/tags/v2"))
                (ref (handler-bind ((rev:tag-immutable #'%replacing))
                       (rev:tag-revision s "v2" (getf k :c2) :annotation "release two, fixed"))))
            (ok (equal "release two, fixed" (rev:ref-annotation ref)))
            (ok (equal (getf k :c2) (rev:ref-target ref)))
            (ok (not (equal old (ref-id r "refs/tags/v2"))) "a new tag object")
            (ok (equal "tag" (git r "cat-file" "-t" "refs/tags/v2")))
            (ok (equal (getf k :c2) (rev r "refs/tags/v2")))
            (ok (equal "release two, fixed"
                       (git r "for-each-ref" "--format=%(contents:subject)" "refs/tags/v2")))
            (ok (equal "commit v2" (git r "for-each-ref" "--format=%(*objecttype) %(tag)"
                                        "refs/tags/v2"))
                "a tag of the commit, named v2")))
        (testing "lightweight → annotated, and back"
          (handler-bind ((rev:tag-immutable #'%replacing))
            (rev:tag-revision s "v1" (getf k :c1) :annotation "now annotated"))
          (ok (equal "tag" (git r "cat-file" "-t" "refs/tags/v1")))
          (ok (equal "now annotated" (rev:ref-annotation (rev:find-ref s "v1"))))
          (handler-bind ((rev:tag-immutable #'%replacing))
            (rev:tag-revision s "v1" (getf k :c1)))
          (ok (equal "commit" (git r "cat-file" "-t" "refs/tags/v1")))
          (ok (null (rev:ref-annotation (rev:find-ref s "v1")))))
        (ok (null events) "no listener event: the target did not move")))))
