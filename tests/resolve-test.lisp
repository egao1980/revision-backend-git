(in-package #:revision-backend-git/tests)

;;; RESOLVE-REVISION and the revision values: ids, prefixes, revspecs, refs,
;;; tags, parent / ancestor designators; revision fields against git.

(deftest revision-fields-match-git
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (dolist (name '(:c1 :c2 :c3))
          (let* ((id (getf k name))
                 (rv (rev:resolve-revision s id)))
            (ok (rev:revision-p rv))
            (ok (equal id (rev:revision-id rv)) "id = full commit hash")
            (ok (equal (remove "" (uiop:split-string (git r "log" "-1" "--format=%P" id)
                                                     :separator '(#\Space))
                               :test #'string=)
                       (rev:revision-parent-ids rv)))
            (ok (= (parse-integer (git r "log" "-1" "--format=%ct" id))
                   (unix (rev:revision-recorded-at rv)))
                "recorded-at = committer date")
            (ok (= (parse-integer (git r "log" "-1" "--format=%at" id))
                   (unix (rev:revision-valid-at rv)))
                "valid-at = author date")
            (ok (equal (git r "rev-parse" (format nil "~a^{tree}" id)) (rev:revision-content rv))
                "content = tree id")
            (ok (plist-matches-p (list :author-name "Test User" :author-email "test@example.invalid"
                                       :committer-name "Test User"
                                       :committer-email "test@example.invalid"
                                       :tree (git r "rev-parse" (format nil "~a^{tree}" id)))
                                 (rev:revision-provenance rv)))))
        (ok (equal "second" (rev:revision-message (rev:resolve-revision s (getf k :c2))))
            "message without its final newline")
        (ok (equal (format nil "third~%~%with a body")
                   (rev:revision-message (rev:resolve-revision s (getf k :c3)))))
        (ok (= (- +t0+ 10000) (unix (rev:revision-time (rev:resolve-revision s (getf k :c2)) :valid))))))))

(deftest merge-parents-in-order
  (with-test-repo (r)
    (let ((k (build-merge r)))
      (with-store (s r)
        (ok (equal (list (getf k :m1) (getf k :f2))
                   (rev:revision-parent-ids (rev:resolve-revision s (getf k :x)))))
        (ok (null (rev:revision-parent-ids (rev:resolve-revision s (getf k :base)))) "root")))))

(deftest resolve-id-prefixes
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (ok (equal (getf k :c2) (rev:revision-id (rev:resolve-revision s (subseq (getf k :c2) 0 7)))))
        (ok (equal (getf k :c1) (rev:revision-id (rev:resolve-revision s (subseq (getf k :c1) 0 12)))))
        (let ((rv (rev:resolve-revision s (getf k :c3))))
          (ok (equal (getf k :c3) (rev:revision-id (rev:resolve-revision s rv))) "a revision designates itself"))))))

(deftest resolve-ambiguous-prefix
  (with-test-repo (r)
    (multiple-value-bind (prefix candidates) (build-ambiguous r)
      (with-store (s r)
        (let ((c (capture-condition (rev:ambiguous-revision) (rev:resolve-revision s prefix))))
          (ok (typep c 'rev:ambiguous-revision))
          (when c
            (ok (equal prefix (rev:ambiguous-revision-designator c)))
            (ok (set= candidates (rev:ambiguous-revision-candidates c))
                "candidates = every commit with the prefix")))
        (testing "a longer prefix and an unreachable commit resolve"
          (let ((one (first candidates)))
            (ok (equal one (rev:revision-id (rev:resolve-revision s (subseq one 0 12)))))))))))

(deftest-parametrize resolve-not-found
    ((designator) :ids ("absent-full-id" "absent-prefix" "too-short" "junk" "no-branch" "no-tag")
     ("deadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
     ("deadbeef")
     ("abc")
     ("not a revspec ^^")
     ((rev:rev-ref "nope"))
     ((rev:rev-tag "nope")))
  (with-test-repo (r)
    (build-linear r)
    (with-store (s r)
      (ok (signals (rev:resolve-revision s designator) 'rev:revision-not-found)))))

(deftest-parametrize resolve-revspecs
    ((spec expected) :ids ("HEAD" "tilde" "caret" "branch" "tag-name" "peeled" "tree-of")
     ("HEAD" :c3)
     ("main~2" :c1)
     ("HEAD^" :c2)
     ("main" :c3)
     ("v2" :c2)
     ("v2^{}" :c2)
     ("v1^{commit}" :c1))
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (ok (equal (getf k expected) (rev:revision-id (rev:resolve-revision s spec))))))))

(deftest resolve-refs-and-tags
  (with-test-repo (r)
    (let ((k (build-linear r)))
      (with-store (s r)
        (flet ((id (d) (rev:revision-id (rev:resolve-revision s d))))
          (ok (equal (getf k :c3) (id (rev:rev-ref "main"))))
          (ok (equal (getf k :c2) (id (rev:rev-ref "v2"))) "annotated tag peels to its commit")
          (ok (equal (getf k :c1) (id (rev:rev-tag "v1"))))
          (ok (equal (getf k :c2) (id (rev:rev-tag "v2"))))
          (ok (signals (rev:resolve-revision s (rev:rev-tag "main")) 'rev:revision-not-found)
              "a branch is not a tag")
          (testing "branch and tag of the same name: the tag wins"
            (git* r (list "branch" "dup" (getf k :c1)))
            (git* r (list "tag" "dup" (getf k :c2)))
            (ok (equal (getf k :c2) (id (rev:rev-ref "dup"))))))))))

(deftest resolve-parent-and-ancestor
  (with-test-repo (r)
    (let ((k (build-merge r)))
      (with-store (s r)
        (flet ((id (d) (rev:revision-id (rev:resolve-revision s d))))
          (ok (equal (getf k :m1) (id (rev:rev-parent (rev:rev-ref "main")))))
          (ok (equal (getf k :f2) (id (rev:rev-parent (getf k :x) 2))))
          (ok (signals (rev:resolve-revision s (rev:rev-parent (getf k :x) 3)) 'rev:revision-not-found))
          (ok (signals (rev:resolve-revision s (rev:rev-parent (getf k :base))) 'rev:revision-not-found))
          (ok (equal (getf k :x) (id (rev:rev-ancestor (rev:rev-ref "main") 0))))
          (ok (equal (getf k :base) (id (rev:rev-ancestor (rev:rev-ref "main") 2))))
          (ok (equal (getf k :f1) (id (rev:rev-ancestor (rev:rev-parent (getf k :x) 2) 1))))
          (ok (signals (rev:resolve-revision s (rev:rev-ancestor (rev:rev-ref "main") 3))
                       'rev:revision-not-found)))))))
