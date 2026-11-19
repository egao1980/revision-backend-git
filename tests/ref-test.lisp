(in-package #:revision-backend-git/tests)

;;; Refs and tags: listing, CAS through update-ref, restarts, tag rules,
;;; the listener; every update is visible to (and checked against) real git.

(defparameter +zero+ "0000000000000000000000000000000000000000")

(defun ref-id (repo full-name)
  "Real git: the object a full ref name points at, or NIL."
  (multiple-value-bind (out code)
      (git* repo (list "rev-parse" "--verify" "--quiet" full-name) :ok '(0 1))
    (when (zerop code) (string-right-trim '(#\Newline) out))))

(deftest find-ref-and-list-refs
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (git* r (list "branch" "feature/x" (getf k :c1)))
      (git* r (list "tag" "-a" "tree-tag" "-m" "a tree" (format nil "~a^{tree}" (getf k :c1))))
      (with-store (s r)
        (ok (null (rev:find-ref s "nope")))
        (let ((main (rev:find-ref s "main")))
          (ok (rev:revision-ref-p main))
          (ok (eq :movable (rev:ref-kind main)))
          (ok (equal (getf k :c3) (rev:ref-target main)))
          (ok (null (rev:ref-annotation main))))
        (let ((v1 (rev:find-ref s "v1"))
              (v2 (rev:find-ref s "v2")))
          (ok (eq :tag (rev:ref-kind v1)))
          (ok (null (rev:ref-annotation v1)) "lightweight tag")
          (ok (equal (getf k :c2) (rev:ref-target v2)) "target is the peeled commit")
          (ok (equal "release two" (rev:ref-annotation v2))))
        (ok (equal '("feature/x" "main" "v1" "v2") (mapcar #'rev:ref-name (rev:list-refs s)))
            "sorted; a tag of a tree is left out")
        (ok (equal '("feature/x" "main") (mapcar #'rev:ref-name (rev:list-refs s :kind :movable))))
        (ok (equal '("v1" "v2") (mapcar #'rev:ref-name (rev:list-refs s :kind :tag))))
        (ok (equal '("feature/x") (mapcar #'rev:ref-name (rev:list-refs s :prefix "feature/"))))))))

(deftest move-ref-updates-git
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (let ((ref (rev:move-ref s "topic" (getf k :c1))))
          (ok (equal "topic" (rev:ref-name ref)))
          (ok (eq :movable (rev:ref-kind ref)))
          (ok (equal (getf k :c1) (rev:ref-target ref))))
        (ok (equal (getf k :c1) (ref-id r "refs/heads/topic")) "a real branch")
        (rev:move-ref s "topic" (rev:rev-ref "main"))
        (ok (equal (getf k :c3) (ref-id r "refs/heads/topic")))
        (ok (signals (rev:move-ref s "topic" "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
                     'rev:revision-not-found))
        (ok (equal (getf k :c3) (ref-id r "refs/heads/topic")) "unchanged after a failure")))))

(deftest move-ref-cas
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (testing "matching expected"
          (rev:move-ref s "main" (getf k :c2) :expected (getf k :c3))
          (ok (equal (getf k :c2) (ref-id r "refs/heads/main"))))
        (testing "expected NIL: create only"
          (rev:move-ref s "fresh" (getf k :c1) :expected nil)
          (ok (equal (getf k :c1) (ref-id r "refs/heads/fresh")))
          (ok (signals (rev:move-ref s "fresh" (getf k :c2) :expected nil) 'rev:ref-conflict)))
        (testing "stale expected"
          (let ((c (capture-condition (rev:ref-conflict)
                     (rev:move-ref s "main" (getf k :c1) :expected (getf k :c3)))))
            (ok (typep c 'rev:ref-conflict))
            (when c
              (ok (equal "main" (rev:ref-conflict-name c)))
              (ok (equal (getf k :c3) (rev:ref-conflict-expected c)))
              (ok (equal (getf k :c2) (rev:ref-conflict-actual c)))))
          (ok (equal (getf k :c2) (ref-id r "refs/heads/main"))))
        (testing "another git process moved the ref"
          (git* r (list "update-ref" "refs/heads/main" (getf k :c1)))
          (let ((c (capture-condition (rev:ref-conflict)
                     (rev:move-ref s "main" (getf k :c3) :expected (getf k :c2)))))
            (ok (typep c 'rev:ref-conflict))
            (when c (ok (equal (getf k :c1) (rev:ref-conflict-actual c))))))))))

(deftest ref-conflict-restarts
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (testing "restarts offered (no force by default)"
          (let ((names (restart-names-in 'rev:ref-conflict
                                         (lambda () (rev:move-ref s "main" (getf k :c1)
                                                                  :expected (getf k :c2))))))
            (ok (member 'rev:use-current names))
            (ok (member 'rev:retry names))
            (ok (not (member 'rev:force names)))))
        (testing "use-current returns the current ref unchanged"
          (let ((result (handler-bind ((rev:ref-conflict
                                         (lambda (c) (declare (ignore c)) (invoke-restart 'rev:use-current))))
                          (rev:move-ref s "main" (getf k :c1) :expected (getf k :c2)))))
            (ok (equal (getf k :c3) (rev:ref-target result)))
            (ok (equal (getf k :c3) (ref-id r "refs/heads/main")))))
        (testing "retry moves with expected = current"
          (let ((result (handler-bind ((rev:ref-conflict
                                         (lambda (c) (declare (ignore c)) (invoke-restart 'rev:retry))))
                          (rev:move-ref s "main" (getf k :c1) :expected (getf k :c2)))))
            (ok (equal (getf k :c1) (rev:ref-target result)))
            (ok (equal (getf k :c1) (ref-id r "refs/heads/main")))))
        (testing "commit-revision on a moved ref: use-current returns the tip"
          (let ((result (handler-bind ((rev:ref-conflict
                                         (lambda (c) (declare (ignore c)) (invoke-restart 'rev:use-current))))
                          (rev:commit-revision s "main^{tree}" :ref "main"
                                               :expected (getf k :c3) :message "lost"))))
            (ok (equal (getf k :c1) (rev:revision-id result)))
            (ok (equal (getf k :c1) (ref-id r "refs/heads/main")) "nothing moved")))))))

(deftest force-restart-is-gated
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (let ((s (open-store r :permit-force (lambda (name) (string= name "scratch")))))
        (unwind-protect
             (progn
               (ok (rev:permit-force-p s "scratch"))
               (ok (not (rev:permit-force-p s "main")))
               (rev:move-ref s "scratch" (getf k :c1))
               (ok (member 'rev:force
                           (restart-names-in 'rev:ref-conflict
                                             (lambda () (rev:move-ref s "scratch" (getf k :c2)
                                                                      :expected (getf k :c3))))))
               (ok (not (member 'rev:force
                                (restart-names-in 'rev:ref-conflict
                                                  (lambda () (rev:move-ref s "main" (getf k :c2)
                                                                           :expected (getf k :c1)))))))
               (let ((result (handler-bind ((rev:ref-conflict
                                              (lambda (c) (declare (ignore c)) (invoke-restart 'rev:force))))
                               (rev:move-ref s "scratch" (getf k :c2) :expected (getf k :c3)))))
                 (ok (equal (getf k :c2) (rev:ref-target result)))
                 (ok (equal (getf k :c2) (ref-id r "refs/heads/scratch")))))
          (git:close-git-store s)))
      (with-store (s r)
        (ok (not (rev:permit-force-p s "scratch")) "default: never")))))

(deftest tags-never-move
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (ok (signals (rev:move-ref s "v1" (getf k :c2)) 'rev:tag-immutable))
        (ok (signals (rev:delete-ref s "v1") 'rev:tag-immutable))
        (ok (signals (rev:commit-revision s "main^{tree}" :ref "v1") 'rev:tag-immutable))
        (ok (signals (rev:tag-revision s "v1" (getf k :c3)) 'rev:tag-immutable))
        (ok (signals (rev:tag-revision s "v2" (getf k :c2) :annotation "other words") 'rev:tag-immutable))
        (let ((c (capture-condition (rev:tag-immutable) (rev:move-ref s "v1" (getf k :c2)))))
          (when c
            (ok (equal "v1" (rev:tag-immutable-name c)))
            (ok (equal (getf k :c1) (rev:tag-immutable-target c)))))
        (ok (equal (getf k :c1) (rev r "v1")) "v1 still at c1")))))

(deftest tag-revision-creates-git-tags
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (let ((light (rev:tag-revision s "light" (getf k :c3))))
          (ok (eq :tag (rev:ref-kind light)))
          (ok (equal (getf k :c3) (rev:ref-target light)))
          (ok (null (rev:ref-annotation light)))
          (ok (equal "commit" (git r "cat-file" "-t" "refs/tags/light")) "lightweight"))
        (let ((ann (rev:tag-revision s "ann" (rev:rev-ref "main") :annotation "shipped")))
          (ok (equal "shipped" (rev:ref-annotation ann)))
          (ok (equal "tag" (git r "cat-file" "-t" "refs/tags/ann")) "annotated tag object")
          (ok (equal (getf k :c3) (rev r "refs/tags/ann"))))
        (testing "idempotent with the same target and annotation"
          (let ((again (rev:tag-revision s "ann" (getf k :c3) :annotation "shipped")))
            (ok (equal "shipped" (rev:ref-annotation again)))
            (ok (equal (ref-id r "refs/tags/ann")
                       (progn (rev:tag-revision s "ann" (getf k :c3) :annotation "shipped")
                              (ref-id r "refs/tags/ann")))
                "the tag object is not rewritten")))
        (testing "a tag over an existing branch name"
          (let ((c (capture-condition (rev:ref-conflict) (rev:tag-revision s "main" (getf k :c1)))))
            (ok (typep c 'rev:ref-conflict))
            (when c
              (ok (null (rev:ref-conflict-expected c)))
              (ok (equal (getf k :c3) (rev:ref-conflict-actual c))))))
        (testing "create-new-tag"
          (let ((names (restart-names-in 'rev:tag-immutable
                                         (lambda () (rev:tag-revision s "v1" (getf k :c3))))))
            (ok (member 'rev:create-new-tag names)))
          (let ((result (handler-bind ((rev:tag-immutable
                                         (lambda (c) (declare (ignore c))
                                           (invoke-restart 'rev:create-new-tag "v1-again"))))
                          (rev:move-ref s "v1" (getf k :c3)))))
            (ok (equal "v1-again" (rev:ref-name result)))
            (ok (eq :tag (rev:ref-kind result)))
            (ok (equal (getf k :c3) (ref-id r "refs/tags/v1-again")))
            (ok (equal (getf k :c1) (ref-id r "refs/tags/v1")))))))))

(deftest delete-ref-cas
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (rev:move-ref s "a" (getf k :c1))
        (rev:move-ref s "b" (getf k :c2))
        (ok (eq t (rev:delete-ref s "a")))
        (ok (null (ref-id r "refs/heads/a")))
        (ok (signals (rev:delete-ref s "a") 'rev:revision-not-found))
        (ok (signals (rev:delete-ref s "b" :expected (getf k :c1)) 'rev:ref-conflict))
        (ok (equal (getf k :c2) (ref-id r "refs/heads/b")))
        (ok (null (handler-bind ((rev:ref-conflict
                                   (lambda (c) (declare (ignore c)) (invoke-restart 'rev:use-current))))
                    (rev:delete-ref s "b" :expected (getf k :c1))))
            "use-current → NIL, ref kept")
        (ok (equal (getf k :c2) (ref-id r "refs/heads/b")))
        (ok (eq t (rev:delete-ref s "b" :expected (getf k :c2))))
        (ok (null (ref-id r "refs/heads/b")))))))

(deftest ref-listener-events
  (with-test-repo (r)
    (let ((k (build-linear r))
          (events '()))
      (with-store (s r)
        (ok (null (rev:ref-listener s)))
        (setf (rev:ref-listener s) (lambda (e) (push e events)))
        (rev:move-ref s "topic" (getf k :c1))
        (rev:move-ref s "topic" (getf k :c2))
        (ignore-errors (rev:move-ref s "topic" (getf k :c3) :expected (getf k :c1)))
        (rev:tag-revision s "t1" (getf k :c3))
        (rev:delete-ref s "topic")
        (let ((new (rev:commit-revision s "HEAD^{tree}" :ref "main" :message "c4")))
          (ok (equal (list (list :ref-moved "topic" nil (getf k :c1))
                           (list :ref-moved "topic" (getf k :c1) (getf k :c2))
                           (list :ref-moved "t1" nil (getf k :c3))
                           (list :ref-moved "topic" (getf k :c2) nil)
                           (list :ref-moved "main" (getf k :c3) (rev:revision-id new)))
                     (reverse events))
              "one event per successful change, none for the failed CAS"))
        (git* r (list "update-ref" "refs/heads/external" (getf k :c1)))
        (ok (= 5 (length events)) "changes by other git processes are not reported")
        (setf (rev:ref-listener s) nil)
        (ok (null (rev:ref-listener s)))))))
