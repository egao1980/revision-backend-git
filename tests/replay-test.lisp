(in-package #:revision-backend-git/tests)

;;; V1 REPLAY-REVISIONS (`git replay`), the one capability-gated operation:
;;; it needs git >= *REPLAY-GIT-VERSION* (2.44.0) while the store minimum
;;; stays 2.43.0 (ci-base, cl-repository#101). On an older git the tests
;;; check the gate instead of the replay.

(defun installed-git-version ()
  "The installed git's version list (real `git --version`)."
  (git:parse-git-version (string-right-trim '(#\Newline)
                                            (uiop:run-program (list *git* "--version")
                                                              :output :string))))

(defun version>= (a b)
  "A and B version lists (PARSE-GIT-VERSION) → A >= B."
  (loop for x in a for y in b
        when (> x y) return t
        when (< x y) return nil
        finally (return (>= (length a) (length b)))))

(defun replay-available-p ()
  (version>= (installed-git-version) (git:parse-git-version git:*replay-git-version*)))

(defun build-feature (repo)
  "main: m0 ← m1 (edits main.txt); feature from m0: f1 (adds f1.txt), f2
(edits f1.txt, author date backdated); conflict from m0: x1 edits main.txt
differently. Checked out: main. → plist."
  (put-file repo "main.txt" (lines "m0"))
  (let ((m0 (commit-all repo "m0" :at +t0+)))
    (git* repo '("checkout" "-q" "-b" "feature"))
    (put-file repo "f1.txt" (lines "f1"))
    (let ((f1 (commit-all repo "f1" :at (+ +t0+ 100))))
      (put-file repo "f1.txt" (lines "f1" "f2"))
      (let ((f2 (commit-all repo (format nil "f2~%~%body") :at (+ +t0+ 200) :author-at (- +t0+ 5000))))
        (git* repo (list "checkout" "-q" "-b" "clash" m0))
        (put-file repo "main.txt" (lines "clash"))
        (let ((x1 (commit-all repo "x1" :at (+ +t0+ 250))))
          (git* repo '("checkout" "-q" "main"))
          (put-file repo "main.txt" (lines "m1"))
          (let ((m1 (commit-all repo "m1" :at (+ +t0+ 300))))
            (list :m0 m0 :m1 m1 :f1 f1 :f2 f2 :x1 x1)))))))

(defun build-merge-commit (repo k)
  "A merge commit of f2 and m1, written with commit-tree (no checkout) → its id."
  (string-right-trim '(#\Newline)
                     (git* repo (list "commit-tree" (tree-of repo (getf k :f2))
                                      "-p" (getf k :f2) "-p" (getf k :m1) "-m" "a merge")
                           :env (date-env (+ +t0+ 500)))))

(deftest replay-version-gate
  (ok (equal "2.44.0" git:*replay-git-version*) "locked")
  (with-test-repo (r)
    (let ((k (build-feature r)))
      (with-store (s r)
        (ok (eq (and (replay-available-p) t) (and (git:git-replay-p s) t))
            "git-replay-p follows the probed version")
        (testing "an older git signals before running anything"
          (let* ((git:*replay-git-version* "99.0.0")
                 (feature (rev r "feature"))
                 (c (capture-condition (git:vc-tool-version-error)
                      (git:replay-revisions s (list (getf k :f1) (getf k :f2)) (getf k :m1)
                                            :ref "feature"))))
            (ok (not (git:git-replay-p s)))
            (ok (typep c 'git:vc-tool-version-error))
            (when c
              (ok (equal "git" (git:vc-tool-version-error-tool c)))
              (ok (equal "99.0.0" (git:vc-tool-version-error-required c)))
              (ok (stringp (git:vc-tool-version-error-found c))))
            (ok (equal feature (rev r "feature")) "nothing moved")))))))

(deftest replay-onto
  (if (not (replay-available-p))
      (skip "git < 2.44: replay-version-gate covers this git")
      (with-test-repo (r)
        (let ((k (build-feature r)))
          (with-store (s r)
            (let ((new (with-checkout-unchanged (r "replay-revisions")
                         (dt:with-fixed-clock ((instant (+ +t0+ 900)))
                           (git:replay-revisions s (rev:revision-range s "feature" "main") (getf k :m1)
                                                 :ref "feature")))))
              (ok (= 2 (length new)))
              (ok (equal (list (getf k :m1)) (rev:revision-parent-ids (first new))) "onto m1")
              (ok (equal (list (rev:revision-id (first new))) (rev:revision-parent-ids (second new)))
                  "parents first, one chain")
              (ok (equal (list "f1" (format nil "f2~%~%body")) (mapcar #'rev:revision-message new)) "messages kept")
              (ok (= (- +t0+ 5000) (unix (rev:revision-valid-at (second new)))) "author date kept")
              (ok (equal (rev:revision-id (second new)) (rev r "feature")) "ref moved")
              (ok (equal (lines "m1") (blob-text r (rev:revision-id (second new)) "main.txt")))
              (ok (equal (lines "f1" "f2") (blob-text r (rev:revision-id (second new)) "f1.txt")))
              (ok (rev:ancestor-p s (getf k :m1) (second new)))
              (ok (null (git:replay-revisions s nil (getf k :m1))) "nothing to replay")))))))

(deftest replay-refusals
  (if (not (replay-available-p))
      (skip "git < 2.44: replay-version-gate covers this git")
      (with-test-repo (r)
        (let ((k (build-feature r)))
          (with-store (s r)
            (testing "conflict: REPLAY-CONFLICT, no ref moved"
              (let* ((clash (rev r "clash"))
                     (c (capture-condition (git:replay-conflict)
                          (git:replay-revisions s (list (getf k :x1)) (getf k :m1) :ref "clash"))))
                (ok (typep c 'git:replay-conflict))
                (ok (typep c 'rev:revision-error))
                (when c
                  (ok (equal (getf k :m1) (git:replay-conflict-onto c)))
                  (ok (member (git:replay-conflict-revision c) (list nil (getf k :x1)) :test #'equal)))
                (ok (equal clash (rev r "clash")))))
            (testing "ref CAS"
              (let ((c (capture-condition (rev:ref-conflict)
                         (git:replay-revisions s (list (getf k :f1) (getf k :f2)) (getf k :m1)
                                               :ref "feature" :expected (getf k :f1)))))
                (ok (typep c 'rev:ref-conflict))
                (ok (equal (getf k :f2) (rev r "feature")) "unchanged")))
            (testing "not a chain"
              (ok (signals (git:replay-revisions s (list (getf k :f2) (getf k :f1)) (getf k :m1))
                           'rev:revision-error)
                  "children before parents")
              (ok (signals (git:replay-revisions s (list (getf k :f2) (getf k :x1)) (getf k :m1))
                           'rev:revision-error)
                  "unrelated commits"))
            (testing "merge commits are refused"
              (let ((merge (build-merge-commit r k)))
                (ok (signals (git:replay-revisions s (list (getf k :f1) (getf k :f2) merge) (getf k :m1))
                             'rev:revision-error)))))))))

