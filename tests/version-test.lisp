(in-package #:revision-backend-git/tests)

;;; The git program: version parsing and the minimum, the version error,
;;; the store's paths, RUN-GIT (fixed environment / options, errors, I/O),
;;; and the batch process lifecycle.

(defun real-git-version ()
  "Numeric dotted version of the real git, parsed here independently."
  (let* ((out (string-trim '(#\Newline #\Return #\Space)
                           (with-test-repo (r) (git* r '("--version") :directory nil))))
         (start (length "git version "))
         (end (or (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.)))
                                   out :start start)
                  (length out)))
         (parts (remove "" (uiop:split-string (subseq out start end) :separator '(#\.))
                        :test #'string=)))
    ;; The leading numeric components (2.47.1.windows.2 → 2.47.1).
    (format nil "~{~a~^.~}" parts)))

(deftest-parametrize parse-git-version
    ((input expected)
     :ids ("apple" "windows" "linux-newline" "bare-3" "bare-2" "four-numeric" "nonsense" "empty")
     ("git version 2.54.0 (Apple Git-157)" '(2 54 0))
     ("git version 2.47.1.windows.2" '(2 47 1))
     ((format nil "git version 2.43.0~%") '(2 43 0))
     ("2.43.0" '(2 43 0))
     ("2.43" '(2 43))
     ("1.8.3.1" '(1 8 3 1))
     ("not a version" nil)
     ("" nil))
  (ok (equal expected (git:parse-git-version input))))

(deftest minimum-git-version-is-locked
  (ok (equal "2.43.0" git:*minimum-git-version*)
      "V0 minimum = the oldest git on the CI runners (ci-base, Ubuntu 24.04)")
  (ok (equal "git" git:*git-program*)))

(deftest installed-git-meets-the-minimum
  (let ((found (git:parse-git-version (real-git-version)))
        (need (git:parse-git-version git:*minimum-git-version*)))
    (ok (and found need
             (loop for a in (append found '(0 0 0))
                   for b in (append need '(0 0 0))
                   repeat 4
                   do (cond ((> a b) (return t)) ((< a b) (return nil)))
                   finally (return t)))
        (format nil "git ~a on this runner >= ~a" (real-git-version) git:*minimum-git-version*))))

(deftest git-version-of-a-store
  (with-test-repo (r)
    (with-store (s r)
      (ok (equal (real-git-version) (git:git-version s))))))

(deftest too-old-git-signals-vc-tool-version-error
  (with-test-repo (r)
    (let ((c (let ((git:*minimum-git-version* "999.0.0"))
               (capture-condition (git:vc-tool-version-error) (open-store r)))))
      (ok (typep c 'git:vc-tool-version-error))
      (ok (typep c 'rev:revision-error) "a revision-error")
      (when c
        (ok (equal "git" (git:vc-tool-version-error-tool c)))
        (ok (equal (real-git-version) (git:vc-tool-version-error-found c)))
        (ok (equal "999.0.0" (git:vc-tool-version-error-required c)))
        (ok (search "999.0.0" (princ-to-string c)) "report names the requirement")))
    (testing "the check reads the variable at call time"
      (ok (git:git-store-p (open-store r))))))

(deftest missing-git-signals-vc-tool-version-error
  (with-test-repo (r)
    (let ((c (capture-condition (git:vc-tool-version-error)
               (open-store r :git (uiop:native-namestring
                                   (merge-pathnames "no-such-git-program" (trepo-root r)))))))
      (ok (typep c 'git:vc-tool-version-error))
      (when c
        (ok (null (git:vc-tool-version-error-found c)))
        (ok (equal git:*minimum-git-version* (git:vc-tool-version-error-required c)))))))

(deftest vc-tool-version-error-is-the-protocol-condition
  ;; revision-protocol#4: one condition for the git and jj backends, re-exported here.
  (ok (eq 'git:vc-tool-version-error 'rev:vc-tool-version-error))
  (ok (eq 'git:vc-tool-version-error-tool 'rev:vc-tool-version-error-tool))
  (ok (eq 'git:vc-tool-version-error-found 'rev:vc-tool-version-error-found))
  (ok (eq 'git:vc-tool-version-error-required 'rev:vc-tool-version-error-required))
  (ok (eq :external (nth-value 1 (find-symbol "VC-TOOL-VERSION-ERROR" "REVISION-BACKEND-GIT")))))

(deftest capability-queries
  ;; revision-protocol#5
  (with-test-repo (r)
    (with-store (s r)
      (ok (eq t (rev:ref-cas-p s)) ":ref-cas T: update-ref compares and swaps")
      (ok (eq nil (rev:conflicted-revisions-p s)) ":conflicted-revisions NIL"))))

(deftest store-paths
  (with-test-repo (r)
    (put-file r "marker.txt" "m")
    (put-file r "sub/deeper/x.txt" "x")
    (with-store (s r)
      (let ((top (git:git-store-path s))
            (gd (git:git-store-git-dir s)))
        (ok (and (pathnamep top) (uiop:absolute-pathname-p top) (uiop:directory-pathname-p top)))
        (ok (probe-file (merge-pathnames "marker.txt" top)) "top-level of the worktree")
        (ok (and (pathnamep gd) (uiop:directory-pathname-p gd)))
        (ok (probe-file (merge-pathnames "HEAD" gd)) "the git dir")
        (ok (not (git:git-store-bare-p s)))))
    (testing "a subdirectory opens the same worktree"
      (let ((s (git:make-git-store (repo-file r "sub/deeper/") :env (store-env r))))
        (ok (probe-file (merge-pathnames "marker.txt" (git:git-store-path s))))
        (git:close-git-store s)))))

(deftest bare-store-paths
  (with-test-repo (r :bare t)
    (with-store (s r)
      (ok (git:git-store-bare-p s))
      (ok (null (git:git-store-path s)))
      (ok (probe-file (merge-pathnames "HEAD" (git:git-store-git-dir s)))))))

(deftest not-a-repository-signals-git-command-error
  (with-test-repo (r)
    (let* ((elsewhere (merge-pathnames "not-a-repo/" (trepo-root r))))
      (ensure-directories-exist elsewhere)
      (let ((c (capture-condition (git:git-command-error)
                 (git:make-git-store elsewhere
                                     :env (append (store-env r)
                                                  (list (cons "GIT_CEILING_DIRECTORIES"
                                                              (uiop:native-namestring (trepo-root r)))))))))
        (ok (typep c 'git:git-command-error))
        (when c
          (ok (integerp (git:git-command-error-exit-code c)))
          (ok (not (eql 0 (git:git-command-error-exit-code c))))
          (ok (plusp (length (git:git-command-error-stderr c)))))))))

(deftest store-env-is-a-copy
  (with-test-repo (r)
    (with-store (s r)
      (let ((env (git:git-store-env s)))
        (ok (equal (store-env r) env))
        (setf (cdr (first env)) "mallory")
        (ok (equal (store-env r) (git:git-store-env s)))))))

(deftest run-git-output-and-exit-code
  (with-test-repo (r)
    (let ((c1 (getf (build-linear r) :c1)))
      (with-store (s r)
        (multiple-value-bind (out code err) (git:run-git s (list "rev-parse" "--verify" "v1^{commit}"))
          (ok (equal (format nil "~a~%" c1) out))
          (ok (eql 0 code))
          (ok (stringp err)))
        (testing "ok-exit-codes accepts other statuses without signalling"
          (multiple-value-bind (out code)
              (git:run-git s '("merge-base" "--is-ancestor" "main" "v1") :ok-exit-codes '(0 1))
            (ok (equal "" out))
            (ok (eql 1 code))))))))

(deftest run-git-failure-signals-git-command-error
  (with-test-repo (r)
    (build-linear r)
    (with-store (s r)
      (let* ((args '("rev-parse" "--verify" "no-such-thing"))
             (c (capture-condition (git:git-command-error) (git:run-git s args))))
        (ok (typep c 'git:git-command-error))
        (when c
          (ok (equal args (git:git-command-error-command c)))
          (ok (eql 128 (git:git-command-error-exit-code c)))
          (ok (search "fatal" (git:git-command-error-stderr c)))
          (ok (search "rev-parse" (princ-to-string c)) "report names the command"))))))

(deftest run-git-octets-and-input
  (with-test-repo (r)
    (let ((blob (hash-blob r +bin-v1+)))
      (with-store (s r)
        (ok (equalp +bin-v1+ (git:run-git s (list "cat-file" "blob" blob) :output :octets))
            "exact bytes (NUL and 255)")
        (ok (equal (format nil "~a~%" (hash-blob r "abc"))
                   (git:run-git s '("hash-object" "--stdin") :input "abc"))
            "string input")
        (ok (equal (format nil "~a~%" blob)
                   (git:run-git s '("hash-object" "--stdin") :input +bin-v1+))
            "octet input")
        (ok (equal (format nil "~a~%" (hash-blob r "café ✓"))
                   (git:run-git s '("hash-object" "--stdin") :input "café ✓"))
            "strings are sent as UTF-8")))))

(defun printenv-alias (names)
  "Global options + an alias that echoes NAMES' values, one per line (runs in
git's sh, also on Windows)."
  (list "-c" (format nil "alias.rbgenv=!printf '%s\\n'~{ \"$~a\"~}" names) "rbgenv"))

(deftest fixed-environment
  (with-test-repo (r)
    (build-linear r)
    (let ((s (git:make-git-store (trepo-dir r)
                                 :env (append (list (cons "LC_ALL" "fr_FR.UTF-8")
                                                    (cons "GIT_TERMINAL_PROMPT" "1")
                                                    (cons "RBG_TEST_VAR" "hello"))
                                              (store-env r)))))
      (unwind-protect
           (progn
             (ok (equal (lines "C" "0" "1" "0" "hello")
                        (git:run-git s (printenv-alias '("LC_ALL" "GIT_TERMINAL_PROMPT"
                                                         "GIT_CONFIG_NOSYSTEM" "GIT_OPTIONAL_LOCKS"
                                                         "RBG_TEST_VAR"))))
                 "fixed variables win over :ENV; other :ENV entries pass through"))
        (git:close-git-store s)))))

(deftest fixed-options
  (with-test-repo (r)
    (build-linear r)
    (git* r '("config" "core.quotepath" "on"))
    (git* r '("config" "core.autocrlf" "true"))
    (git* r '("config" "color.ui" "always"))
    (with-store (s r)
      (ok (equal (lines "off") (git:run-git s '("config" "--get" "core.quotepath"))))
      (ok (equal (lines "false") (git:run-git s '("config" "--get" "core.autocrlf"))))
      (ok (equal (lines "never") (git:run-git s '("config" "--get" "color.ui"))))
      (testing "non-ASCII paths come back unquoted"
        (index-entry r "100644" (hash-blob r "x") "café.txt")
        (ok (equal (lines "café.txt")
                   (git:run-git s '("diff" "--cached" "--name-only"))))))))

(deftest batch-process-lifecycle
  (with-test-repo (r)
    (let ((ids (build-linear r)))
      (let ((s (open-store r)))
        (ok (not (git:git-store-batch-active-p s)) "not started by MAKE-GIT-STORE")
        (rev:revision-state s (getf ids :c2))
        (ok (git:git-store-batch-active-p s) "started by an object read")
        (ok (eq t (git:close-git-store s)))
        (ok (not (git:git-store-batch-active-p s)))
        (ok (eq t (git:close-git-store s)) "idempotent")
        (ok (state= (rev:revision-state s (getf ids :c3)) (rev:revision-state s (getf ids :c3)))
            "usable after close")
        (ok (git:git-store-batch-active-p s) "restarted")
        (git:close-git-store s))
      (let ((kept nil))
        (git:with-git-store (s (trepo-dir r) :env (store-env r))
          (setf kept s)
          (rev:revision-state s (getf ids :c1))
          (ok (git:git-store-batch-active-p s)))
        (ok (not (git:git-store-batch-active-p kept)) "WITH-GIT-STORE closes")))))
