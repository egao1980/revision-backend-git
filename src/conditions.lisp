(in-package #:revision-backend-git)

;;; Conditions. Everything a revision store reports through the protocol
;;; (REVISION-NOT-FOUND, AMBIGUOUS-REVISION, REF-CONFLICT, TAG-IMMUTABLE,
;;; REVISION-ERROR ...) is REVISION-PROTOCOL's own type: the backend signals
;;; those. Only two conditions are git-specific:
;;;
;;;   revision-error
;;;   ├── vc-tool-version-error (tool found required)
;;;   └── git-command-error     (command exit-code stderr)
;;;
;;; VC-TOOL-VERSION-ERROR is shared with revision-backend-jj in spirit; it
;;; moves to revision-protocol when that backend starts (see the brief), and
;;; this package then re-exports the protocol's symbol.

(define-condition vc-tool-version-error (rev:revision-error)
  ((tool :initarg :tool :reader vc-tool-version-error-tool :initform nil)
   (found :initarg :found :reader vc-tool-version-error-found :initform nil)
   (required :initarg :required :reader vc-tool-version-error-required
             :initform nil))
  (:report (lambda (c s)
             (format s "~a ~a is too old or missing: found ~a, need ~a or later"
                     (or (vc-tool-version-error-tool c) "tool")
                     (if (vc-tool-version-error-found c) "version" "program")
                     (or (vc-tool-version-error-found c) "nothing")
                     (vc-tool-version-error-required c))))
  (:documentation "The version-control program TOOL (a string, \"git\") is
older than REQUIRED (a version string), or cannot be run at all. FOUND is the
version string the program reported, or NIL when it could not be started.
Signalled by MAKE-GIT-STORE, before any other git command runs."))

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
