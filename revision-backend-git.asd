(defsystem "revision-backend-git"
  :version "0.1.0"
  :description "revision-protocol backend over the git CLI: commits as revisions, branches and tags as refs with update-ref CAS, graph walks via rev-list, state diffs via diff-tree / git diff and diff-protocol, working-copy drift and capture, a read-only cl-stack-pathlib filesystem per revision"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "revision-protocol" "0.2.0")
               (:version "diff-protocol" "0.2.0")
               (:version "datetime-protocol" "0.1.3")
               (:version "process-protocol" "0.3.0")
               (:version "process-backend-uiop" "0.2.0")
               (:version "cl-stack-pathlib" "0.4.0")
               (:version "encoding-protocol" "0.2.0")
               (:version "bordeaux-threads" "0.9.4"))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "git")
               (:file "parse")
               (:file "store")
               (:file "working-copy")
               (:file "filesystem"))
  :in-order-to ((test-op (test-op "revision-backend-git/tests"))))

(defsystem "revision-backend-git/tests"
  :depends-on ("revision-backend-git" "revision-protocol" "diff-protocol"
               "datetime-protocol" "cl-stack-pathlib" "bordeaux-threads" "babel" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "fixtures")
               (:file "version-test")
               (:file "parse-test")
               (:file "resolve-test")
               (:file "ref-test")
               (:file "commit-test")
               (:file "graph-test")
               (:file "content-test")
               (:file "working-copy-test")
               (:file "filesystem-test")
               (:file "law-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
