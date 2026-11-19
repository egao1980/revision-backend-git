(defpackage #:revision-backend-git/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:git #:revision-backend-git)
                    (#:rev #:revision-protocol)
                    (#:diff #:diff-protocol)
                    (#:dt #:datetime-protocol)
                    (#:pathlib #:cl-stack-pathlib)))
