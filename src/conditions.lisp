(in-package #:revision-backend-git)

;;; Conditions. Everything a revision store reports through the protocol
;;; (REVISION-NOT-FOUND, AMBIGUOUS-REVISION, REF-CONFLICT, TAG-IMMUTABLE,
;;; REVISION-ERROR ...) is REVISION-PROTOCOL's own type: the backend signals
;;; those.
;;;
;;;   revision-error
;;;   ├── vc-tool-version-error (tool found required)   revision-protocol's,
;;;   │                                                  re-exported here
;;;   └── git-command-error     (command exit-code stderr)
;;;
;;; VC-TOOL-VERSION-ERROR (TOOL "git", FOUND the version git reported or NIL
;;; when it could not be started) is signalled by MAKE-GIT-STORE before any
;;; other git command runs. It is the same condition revision-backend-jj
;;; signals (revision-protocol#4); callers that write
;;; STACK-REV-GIT:VC-TOOL-VERSION-ERROR keep working.

(define-condition git-command-error (rev:revision-error)
  ((command :initarg :command :reader git-command-error-command :initform nil)
   (exit-code :initarg :exit-code :reader git-command-error-exit-code
              :initform nil)
   (stderr :initarg :stderr :reader git-command-error-stderr :initform nil))
  (:report (lambda (c s)
             (format s "git ~{~a~^ ~} exited with ~a"
                     (git-command-error-command c)
                     (git-command-error-exit-code c))
             (let ((err (git-command-error-stderr c)))
               (when (and err (plusp (length err)))
                 (format s ": ~a" (string-right-trim '(#\Newline #\Return) err))))))
  (:documentation "A git command exited with a status the caller did not
accept. COMMAND is the argument list after the program name and after the
fixed options (what the caller passed to RUN-GIT), EXIT-CODE the integer
status, STDERR the decoded standard error (a string, possibly empty).
Failures with a protocol meaning (an unknown revision, a stale CAS value) are
translated to the protocol's conditions instead and never surface as
GIT-COMMAND-ERROR."))
