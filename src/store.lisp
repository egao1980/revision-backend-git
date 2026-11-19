(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; REVISION-PROTOCOL methods on GIT-STORE (V0: graph, refs, content).
;;; Git-specific behaviour, beyond the protocol docstrings:
;;;
;;; RESOLVE-REVISION — a string designator is tried, in order, as
;;;   1. a full object name of a commit;
;;;   2. a hex prefix of at least REV:+MINIMUM-ID-PREFIX+ characters matching
;;;      commit objects only (reachable or not): one → that commit; several →
;;;      AMBIGUOUS-REVISION with every matching full commit id as CANDIDATES;
;;;   3. a git revspec (`rev-parse --verify --end-of-options SPEC^{commit}`):
;;;      "HEAD", "main~2", "v1^{}", "main@{1}" ….
;;;   Nothing matches → REVISION-NOT-FOUND. REV-REF / REV-TAG take short names
;;;   (refs/heads/NAME, refs/tags/NAME). An annotated tag resolves to the
;;;   commit it peels to. When a branch and a tag share a name, REV-REF and
;;;   FIND-REF pick the tag (git's refs/tags-before-refs/heads rule).
;;;
;;; Refs — FIND-REF / LIST-REFS cover refs/heads/* (:MOVABLE) and
;;;   refs/tags/* (:TAG) whose target peels to a commit; tags of trees or
;;;   blobs are left out. LIST-REFS sorts by name with STRING<, a branch
;;;   before a tag of the same name. REF-ANNOTATION is the annotated tag's
;;;   message without its final newline, NIL for a lightweight tag or a branch.
;;;   MOVE-REF / DELETE-REF / COMMIT-REVISION :REF use
;;;   `update-ref refs/heads/NAME NEW OLD` (OLD = the zero id for :EXPECTED
;;;   NIL), so CAS also holds against other git processes. TAG-REVISION
;;;   creates a lightweight tag, or an annotated one (`tag -a`) when
;;;   :ANNOTATION is given; the tagger is the committer identity of the
;;;   store's environment. REF-LISTENER sees changes made through this store
;;;   only.
;;;
;;; COMMIT-REVISION — CONTENT is one of:
;;;   a DIFF-PROTOCOL tree value            → that tree (blobs and trees written);
;;;   a string                              → an existing tree: a full tree id or
;;;                                           any revspec naming a tree
;;;                                           ("HEAD^{tree}");
;;;   :INDEX                                → the repository index (`write-tree`;
;;;                                           the index file is not modified);
;;;   a DIFF-PROTOCOL:DIFF                  → applied to the first parent's
;;;                                           state (empty tree for a root);
;;;   a REVISION-PROTOCOL:REVISION-PAYLOAD  → its state (or its delta).
;;;   Anything else → REVISION-ERROR. The committer date is (DT:NOW) and the
;;;   author date VALID-AT or (DT:NOW), both written as whole seconds in UTC,
;;;   so DT:WITH-FIXED-CLOCK makes commit ids deterministic. PROVENANCE keys
;;;   :AUTHOR-NAME :AUTHOR-EMAIL :COMMITTER-NAME :COMMITTER-EMAIL override the
;;;   identity; otherwise git's own (config / environment). MESSAGE (default
;;;   "") is stored with one final newline added when missing. :ID is
;;;   refused with REVISION-ERROR (git chooses ids). On a REF-CONFLICT the new
;;;   commit object may remain in the object database, unreferenced; no ref
;;;   moves.
;;;
;;; Graph — walks use `rev-list` (--topo-order / --date-order /
;;;   --first-parent, then reversed so parents come first); :TIME is committer
;;;   date order. MERGE-BASE uses `merge-base` (--all); ALL NIL returns git's
;;;   single answer. ANCESTOR-P is `merge-base --is-ancestor`. REVISION-RANGE
;;;   is `rev-list INCLUDE… ^EXCLUDE…`. :FOOTPRINT keeps a revision when its
;;;   REVISION-DELTA (against the first parent) overlaps — not git's
;;;   pathspec history simplification, which can drop merges.
;;;   REV-AS-OF :RECORDED / :VALID compare committer / author dates along
;;;   the first-parent line.
;;;
;;; Content — CONTENT-PRIMARY :STATE; CONTENT-ENGINE is a DIFF-PROTOCOL
;;;   TREE-DIFF-ENGINE. REVISION-STATE reads the whole tree into a tree value:
;;;   a blob with a NUL byte in its first 8000 bytes, or that is not valid
;;;   UTF-8, is a :BINARY file with an (unsigned-byte 8) vector; any other
;;;   blob is a :TEXT file with its exact decoded string (CRLF kept). Modes
;;;   are git's (#o100644, #o100755); a symlink is a :TEXT file holding the
;;;   link target, mode #o120000; a submodule entry is a file of type
;;;   :GITLINK whose content is the commit id, mode #o160000.
;;;   REVISION-DELTA of a root is the diff from the empty tree.
;;;
;;; STATE-DIFF — A and B: revision designators or a GIT-WORKING-COPY (its
;;;   TARGET-STATE). LEVEL:
;;;     :SUMMARY  file-level changes from `diff-tree -r -z -M --name-status`
;;;               (:ADD :REMOVE :MODIFY :MOVE; paths are lists of names;
;;;               footprint "tree:/a/b"); BEFORE / AFTER are file nodes read
;;;               lazily (thunks, not forced by STATE-DIFF); no children.
;;;     :CHANGES  (default) the leaves: `git diff -M` → DIFF-PROTOCOL:
;;;               PARSE-DIFF :UNIFIED, i.e. hunks nested under file paths,
;;;               mode changes, moves, whole-file adds / removes.
;;;     :CONTENT  file-level changes with their hunks as CHILDREN.
;;;   A file whose before or after side is :BINARY (by the rule above) or
;;;   :GITLINK, or whose type changes (file <-> symlink <-> gitlink),
;;;   appears as a whole-file change with both file nodes, never as hunks and
;;;   never as an OPAQUE-CHANGE, so every git state-diff applies and inverts.
;;;   Rename detection is git's -M (50%). At every level:
;;;     (state-equal engine (apply-diff (state a) (state-diff s a b)) (state b)).
;;;   Paths git must C-quote even with core.quotepath=off (control
;;;   characters, '"', '\') are unquoted by DIFF-PROTOCOL's PARSE-DIFF
;;;   :UNIFIED (0.2.1), so they get hunks at :CHANGES and :CONTENT too.
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; Ids, objects, revisions
;;; ------------------------------------------------------------------

(defun %object-format (store)
  (or (%git-store-object-format store)
      (setf (%git-store-object-format store)
            (or (%git-line store '("rev-parse" "--show-object-format")) "sha1"))))

(defun %id-length (store)
  (if (string= (%object-format store) "sha256") 64 40))

(defun %zero-id (store)
  (make-string (%id-length store) :initial-element #\0))

(defun %hex-p (string)
  (and (plusp (length string)) (every (lambda (c) (digit-char-p c 16)) string)))

(defun %empty-tree (store)
  "Id of the empty tree (written once per store)."
  (or (%git-store-empty-tree store)
      (setf (%git-store-empty-tree store)
            (%chomp (%git store '("mktree") :input "")))))

(defun %rev-parse-verify (store spec)
  "Full object id SPEC names (`rev-parse --verify --quiet`), or NIL."
  (multiple-value-bind (line code)
      (%git-line store (list "rev-parse" "--verify" "--quiet" "--end-of-options" spec)
                 :ok '(0 1 128))
    (and (eql code 0) line)))

(defun %not-found (designator &optional message)
  (error 'rev:revision-not-found :designator designator :message message))

(defun %parse-person (line)
  "\"Name <email> seconds tz\" → (values name email seconds)."
  (let* ((gt (position #\> line :from-end t))
         (lt (and gt (position #\< line :end gt :from-end t))))
    (if (and gt lt)
        (values (string-right-trim " " (subseq line 0 lt))
                (subseq line (1+ lt) gt)
                (or (parse-integer line :start (1+ gt) :junk-allowed t) 0))
        (values line "" 0))))

(defun %parse-commit (id octets)
  "The commit object OCTETS of ID → REVISION."
  (let* ((text (%utf8-decode octets))
         (sep (search (format nil "~%~%") text))
         (header (subseq text 0 (or sep (length text))))
         (message (if sep (%chomp (subseq text (+ sep 2))) ""))
         (tree nil) (parents '()) author committer)
    (dolist (line (%split-string header #\Newline))
      (let* ((sp (position #\Space line))
             (key (and sp (plusp sp) (subseq line 0 sp)))
             (value (and sp (subseq line (1+ sp)))))
        (cond ((null key))                ; continuation of a multi-line header
              ((string= key "tree") (setf tree value))
              ((string= key "parent") (push value parents))
              ((string= key "author") (setf author value))
              ((string= key "committer") (setf committer value)))))
    (multiple-value-bind (an ae at) (%parse-person (or author ""))
      (multiple-value-bind (cn ce ct) (%parse-person (or committer ""))
        (make-instance 'rev:revision
                       :id id
                       :parent-ids (nreverse parents)
                       :recorded-at (dt:make-instant ct)
                       :valid-at (dt:make-instant at)
                       :provenance (list :author-name an :author-email ae
                                         :committer-name cn :committer-email ce
                                         :tree tree)
                       :message message
                       :content tree)))))

(defun %commit (store id)
  "REVISION of the commit object with full id ID, or NIL (no such commit)."
  (or (bt2:with-lock-held ((%git-store-cache-lock store))
        (gethash id (%git-store-cache store)))
      (multiple-value-bind (type octets) (%batch-object store "contents" id)
        (when (equal type "commit")
          (let ((revision (%parse-commit id octets)))
            (bt2:with-lock-held ((%git-store-cache-lock store))
              (setf (gethash id (%git-store-cache store)) revision)))))))

(defun %object-type (store id)
  (values (%batch-object store "info" id)))

;;; ------------------------------------------------------------------
;;; Resolution
;;; ------------------------------------------------------------------

(defun %branch-target (store name)
  "Commit id refs/heads/NAME points at, or NIL."
  (%rev-parse-verify store (concatenate 'string "refs/heads/" name)))

(defun %tag-target (store name)
  "Commit id the tag NAME peels to, or NIL (no tag, or a tag of a non-commit)."
  (%rev-parse-verify store (concatenate 'string "refs/tags/" name "^{commit}")))

(defun %prefix-commits (store prefix)
  "Full ids of every commit object whose id starts with PREFIX."
  (when (<= (length prefix) (%id-length store))
    (let ((out (%git store (list "rev-parse" (concatenate 'string "--disambiguate=" prefix))
                     :ok '(0 1 128))))
      (remove-duplicates
       (remove-if-not (lambda (id) (and (%hex-p id) (equal "commit" (%object-type store id))))
                      (%lines out))
       :test #'string=))))

(defun %resolve-string (store string top)
  (let ((n (length string))
        (id (string-downcase string)))
    (or (and (= n (%id-length store)) (%hex-p id) (%commit store id))
        (and (>= n rev:+minimum-id-prefix+) (%hex-p id)
             (let ((hits (%prefix-commits store id)))
               (cond ((rest hits)
                      (error 'rev:ambiguous-revision :designator string :candidates hits))
                     (hits (%commit store (first hits))))))
        (let ((spec (%rev-parse-verify store (concatenate 'string string "^{commit}"))))
          (and spec (%commit store spec)))
        (%not-found top))))

(defun %first-parent (store revision)
  (let ((p (first (rev:revision-parent-ids revision))))
    (and p (%commit store p))))

(defun %resolve (store designator &optional (top designator))
  "REVISION named by DESIGNATOR. TOP fills REVISION-NOT-FOUND's designator."
  (etypecase designator
    (string (%resolve-string store designator top))
    (rev:revision
     (or (%commit store (rev:revision-id designator))
         (%not-found top "revision is not in this store")))
    (rev:tag-designator
     (let ((id (%tag-target store (rev:designator-name designator))))
       (or (and id (%commit store id)) (%not-found top))))
    (rev:ref-designator
     (let* ((name (rev:designator-name designator))
            (id (or (%tag-target store name) (%branch-target store name))))
       (or (and id (%commit store id)) (%not-found top))))
    (rev:as-of-designator
     (let ((time (rev:designator-time designator))
           (axis (rev:designator-axis designator)))
       ;; No early stop: valid times are not monotonic along the line.
       (loop for r = (%resolve store (rev:designator-on designator) top)
               then (%first-parent store r)
             while r
             when (dt:<= (rev:revision-time r axis) time)
               return r
             finally (%not-found top "no revision at or before that time"))))
    (rev:parent-designator
     (let* ((base (%resolve store (rev:designator-base designator) top))
            (pid (nth (1- (rev:designator-n designator)) (rev:revision-parent-ids base))))
       (or (and pid (%commit store pid))
           (%not-found top "parent index out of range"))))
    (rev:ancestor-designator
     (let ((r (%resolve store (rev:designator-base designator) top)))
       (dotimes (i (rev:designator-n designator) r)
         (declare (ignorable i))
         (setf r (or (%first-parent store r)
                     (%not-found top "ran past the root"))))))))

(defun %resolve-id (store designator)
  (rev:revision-id (%resolve store designator)))

(defun %ids (store designators)
  "Full ids of DESIGNATORS: NIL, one designator, or a list of them."
  (mapcar (lambda (d) (%resolve-id store d))
          (if (listp designators) designators (list designators))))

(defun %check-designator (x)
  (unless (rev:revision-designator-p x)
    (error 'type-error :datum x :expected-type 'rev:revision-designator))
  x)

(defmethod rev:resolve-revision ((store git-store) designator)
  (%check-designator designator)
  (%resolve store designator))

;;; ------------------------------------------------------------------
;;; Refs
;;; ------------------------------------------------------------------

(defparameter +ref-format+
  "--format=%(refname)%00%(objecttype)%00%(objectname)%00%(*objecttype)%00%(*objectname)%00%(contents)%00"
  "One for-each-ref record: six NUL-terminated fields, then git's LF.")

(defun %ref-entries (store patterns)
  "→ list of (full-name type id peeled-type peeled-id contents)."
  (let ((fields (%split-string (%git store (list* "for-each-ref" +ref-format+ patterns))
                               (code-char 0))))
    (loop while (>= (length fields) 6)
          collect (list (string-left-trim '(#\Newline) (pop fields))
                        (pop fields) (pop fields) (pop fields) (pop fields) (pop fields)))))

(defun %movable-ref (name target)
  (make-instance 'rev:revision-ref :name name :kind :movable :target target))

(defun %entry-ref (store entry)
  "REVISION-REF of a %REF-ENTRIES entry, or NIL (a tag of a non-commit)."
  (destructuring-bind (full type id ptype pid contents) entry
    (flet ((after (prefix)
             (and (> (length full) (length prefix))
                  (string= prefix full :end2 (length prefix))
                  (subseq full (length prefix)))))
      (let ((branch (after "refs/heads/"))
            (tag (after "refs/tags/")))
        (cond
          (branch (and (equal type "commit") (%movable-ref branch id)))
          ((null tag) nil)
          ((equal type "commit")
           (make-instance 'rev:revision-ref :name tag :kind :tag :target id))
          ((equal type "tag")
           (let ((target (cond ((equal ptype "commit") pid)
                               ((equal ptype "tag")
                                (%rev-parse-verify store (concatenate 'string full "^{commit}"))))))
             (and target
                  (make-instance 'rev:revision-ref :name tag :kind :tag :target target
                                                   :annotation (%chomp contents))))))))))

(defun %refs-named (store name)
  "→ (values tag-ref branch-ref) for NAME, each NIL when absent."
  (let ((tag-name (concatenate 'string "refs/tags/" name))
        (branch-name (concatenate 'string "refs/heads/" name))
        (tag nil) (branch nil))
    (dolist (e (%ref-entries store (list tag-name branch-name)))
      (cond ((string= (first e) tag-name) (setf tag (%entry-ref store e)))
            ((string= (first e) branch-name) (setf branch (%entry-ref store e)))))
    (values tag branch)))

(defmethod rev:find-ref ((store git-store) name)
  (check-type name string)
  (multiple-value-bind (tag branch) (%refs-named store name)
    (or tag branch)))

(defmethod rev:list-refs ((store git-store) &key kind prefix)
  (check-type kind (member nil :movable :tag))
  (check-type prefix (or null string))
  (let ((refs (loop for e in (%ref-entries store '("refs/heads" "refs/tags"))
                    for ref = (%entry-ref store e)
                    when (and ref
                              (or (null kind) (eq kind (rev:ref-kind ref)))
                              (or (null prefix)
                                  (let ((n (length prefix)) (name (rev:ref-name ref)))
                                    (and (>= (length name) n) (string= prefix name :end2 n)))))
                      collect ref)))
    (stable-sort refs (lambda (a b)
                        (let ((na (rev:ref-name a)) (nb (rev:ref-name b)))
                          (or (string< na nb)
                              (and (string= na nb)
                                   (eq :movable (rev:ref-kind a))
                                   (eq :tag (rev:ref-kind b)))))))))

;;; Listener

(defmethod rev:ref-listener ((store git-store))
  (%git-store-listener store))

(defmethod (setf rev:ref-listener) (function (store git-store))
  (check-type function (or null function))
  (bt2:with-lock-held ((%git-store-listener-lock store))
    (setf (%git-store-listener store) function)))

(defun %notify (store event)
  "Call STORE's listener with EVENT. No lock held."
  (let ((fn (%git-store-listener store)))
    (when (and fn event)
      (funcall fn event))))

(defmethod rev:permit-force-p ((store git-store) name)
  (let ((gate (%git-store-permit-force store)))
    (if (functionp gate)
        (and (funcall gate name) t)
        (and gate t))))

;;; Compare-and-swap

(defun %update-ref (store full new old)
  "`update-ref FULL NEW OLD` (NEW NIL: delete; OLD NIL: must be absent) →
(values success-p stderr exit-code)."
  (multiple-value-bind (out code err)
      (%git store (if new
                      (list "update-ref" full new (or old (%zero-id store)))
                      (list "update-ref" "-d" full old))
            :ok '(0 1 128))
    (declare (ignore out))
    (values (eql code 0) err code)))

(defun %cas-branch (store name new want want-p)
  "Move refs/heads/NAME to NEW (NIL deletes) when it holds WANT (checked only
when WANT-P). → (values status current): :OK (CURRENT = the old target),
:UNCHANGED, :CONFLICT (CURRENT = the actual target), :MISSING (delete of an
absent ref). Read, compare and update-ref run under the store's ref lock;
update-ref's own OLD check makes it atomic against other processes."
  (let ((full (concatenate 'string "refs/heads/" name))
        (failure nil))
    (bt2:with-lock-held ((%git-store-ref-lock store))
      (loop for attempt below 20
            for cur = (%branch-target store name)
            do (cond ((and (null new) (null cur))
                      (return-from %cas-branch (values :missing nil)))
                     ((and want-p (not (equal want cur)))
                      (return-from %cas-branch (values :conflict cur)))
                     ((equal new cur)
                      (return-from %cas-branch (values :unchanged cur)))
                     (t
                      (multiple-value-bind (ok err code) (%update-ref store full new cur)
                        (when ok
                          (return-from %cas-branch (values :ok cur)))
                        (setf failure (list code err))
                        ;; Another process holds git's ref lock: wait, re-read.
                        (sleep 0.02))))))
    (error 'git-command-error :command (if new (list "update-ref" full new) (list "update-ref" "-d" full))
                              :exit-code (first failure) :stderr (second failure))))

(defun %read-new-tag-name ()
  (format *query-io* "New tag name: ")
  (force-output *query-io*)
  (list (read-line *query-io*)))

(defmethod rev:move-ref ((store git-store) name designator &key (expected nil expected-p))
  (check-type name string)
  (%check-designator designator)
  (when expected (%check-designator expected))
  (let ((forcible (and (rev:permit-force-p store name) t)))
    (loop
      (let ((tag (%tag-target store name)))
        (when tag
          (restart-case (error 'rev:tag-immutable :name name :target tag)
            (rev:create-new-tag (new-name)
              :report "Create a new tag at the designator instead."
              :interactive %read-new-tag-name
              (return (rev:tag-revision store new-name designator))))))
      (let ((target (%resolve-id store designator))
            (want (and expected-p expected (%resolve-id store expected))))
        (multiple-value-bind (status current) (%cas-branch store name target want expected-p)
          (ecase status
            (:unchanged (return (%movable-ref name target)))
            (:ok
             (%notify store (list :ref-moved name current target))
             (return (%movable-ref name target)))
            (:conflict
             (restart-case (error 'rev:ref-conflict :name name :expected want :actual current)
               (rev:use-current ()
                 :report (lambda (s) (format s "Leave ~s unchanged and return it." name))
                 (return (and current (%movable-ref name current))))
               (rev:retry ()
                 :report (lambda (s) (format s "Move ~s with its current target as expected." name))
                 (setf expected current expected-p t))
               (rev:force ()
                 :test (lambda (c) (declare (ignore c)) forcible)
                 :report (lambda (s) (format s "Move ~s unconditionally (EXPECTED omitted)." name))
                 (setf expected nil expected-p nil))))))))))

(defmethod rev:delete-ref ((store git-store) name &key (expected nil expected-p))
  (check-type name string)
  (when expected (%check-designator expected))
  (let ((forcible (and (rev:permit-force-p store name) t)))
    (loop
      (let ((tag (%tag-target store name)))
        (when tag
          (error 'rev:tag-immutable :name name :target tag)))
      (let ((want (and expected-p expected (%resolve-id store expected))))
        (multiple-value-bind (status current) (%cas-branch store name nil want expected-p)
          (ecase status
            (:missing (%not-found name "no ref of that name"))
            (:ok
             (%notify store (list :ref-moved name current nil))
             (return t))
            (:conflict
             (restart-case (error 'rev:ref-conflict :name name :expected want :actual current)
               (rev:use-current ()
                 :report (lambda (s) (format s "Keep ~s; return NIL." name))
                 (return nil))
               (rev:retry ()
                 :report (lambda (s) (format s "Delete ~s whatever it points at now." name))
                 (setf expected current expected-p t))
               (rev:force ()
                 :test (lambda (c) (declare (ignore c)) forcible)
                 :report (lambda (s) (format s "Delete ~s unconditionally (EXPECTED omitted)." name))
                 (setf expected nil expected-p nil))))))))))

(defun %now-seconds ()
  (dt:instant-seconds (dt:now)))

(defmethod rev:tag-revision ((store git-store) name designator &key annotation)
  (check-type name string)
  (check-type annotation (or null string))
  (%check-designator designator)
  (let* ((target (%resolve-id store designator))
         (tag-name (concatenate 'string "refs/tags/" name)))
    (multiple-value-bind (status ref)
        (bt2:with-lock-held ((%git-store-ref-lock store))
          (multiple-value-bind (tag branch) (%refs-named store name)
            (let ((raw-tag (or tag (%rev-parse-verify store tag-name))))
              (cond
                ((and tag (equal target (rev:ref-target tag))
                      (equal annotation (rev:ref-annotation tag)))
                 (values :same tag))
                (raw-tag (values :immutable tag))
                (branch (values :branch branch))
                (annotation
                 (%git store (list "tag" "-a" "--cleanup=verbatim" "-F" "-" name target)
                       :input (if (and (plusp (length annotation))
                                       (char= #\Newline (char annotation (1- (length annotation)))))
                                  annotation
                                  (format nil "~a~%" annotation))
                       :extra-env (list (cons "GIT_COMMITTER_DATE"
                                              (format nil "@~d +0000" (%now-seconds)))))
                 (values :created (rev:find-ref store name)))
                (t
                 (multiple-value-bind (ok err code) (%update-ref store tag-name target nil)
                   (unless ok
                     (error 'git-command-error :command (list "update-ref" tag-name target)
                                               :exit-code code :stderr err)))
                 (values :created (make-instance 'rev:revision-ref :name name :kind :tag
                                                                   :target target)))))))
      (ecase status
        (:same ref)
        (:created
         (%notify store (list :ref-moved name nil target))
         ref)
        (:branch
         (error 'rev:ref-conflict :name name :expected nil :actual (rev:ref-target ref)
                                  :message "a movable ref has that name"))
        (:immutable
         (restart-case (error 'rev:tag-immutable :name name :target (and ref (rev:ref-target ref)))
           (rev:create-new-tag (new-name)
             :report "Create a new tag with another name instead."
             :interactive %read-new-tag-name
             (rev:tag-revision store new-name designator :annotation annotation))))))))

;;; ------------------------------------------------------------------
;;; Writing trees and commits
;;; ------------------------------------------------------------------

(defun %write-blob (store octets)
  (%chomp (%git store '("hash-object" "-w" "--stdin") :input octets)))

(defun %write-tree (store tree)
  "Write the DIFF-PROTOCOL tree value TREE (blobs, then trees) → tree id."
  (let ((input
          (with-output-to-string (s)
            (loop for (name node) in (diff:tree-entries tree)
                  do (multiple-value-bind (mode type id)
                         (if (diff:tree-file-p node)
                             (let ((content (diff:tree-file-content node))
                                   (mode (or (diff:tree-file-mode node) #o100644)))
                               (case (diff:tree-file-type node)
                                 (:gitlink (values #o160000 "commit" content))
                                 (t (values mode "blob"
                                            (%write-blob
                                             store
                                             (typecase content
                                               (string (%utf8-encode content))
                                               ((vector (unsigned-byte 8)) (%octets content))
                                               (t (error 'rev:revision-error
                                                         :message (format nil "file ~s: content ~s is neither text nor octets"
                                                                          name content)))))))))
                             (values #o40000 "tree" (%write-tree store node)))
                       (format s "~o ~a ~a~c~a~c" mode type id #\Tab name (code-char 0)))))))
    (%chomp (%git store '("mktree" "-z") :input input))))

(defun %temp-name (prefix)
  (format nil "~a-~36r" prefix (random (expt 36 10) (make-random-state t))))

(defun %call-with-temp-index (store copy-real-p function)
  "Call FUNCTION with an extra-env alist naming a temporary index file (a
copy of the real one when COPY-REAL-P) → its values. The temporary file and
its lock are deleted afterwards; the real index is never opened for writing."
  (let* ((dir (git-store-git-dir store))
         (temp (merge-pathnames (%temp-name "rbg-index") dir))
         (real (merge-pathnames "index" dir)))
    (unwind-protect
         (progn
           (when (and copy-real-p (probe-file real))
             (uiop:copy-file real temp))
           (funcall function (list (cons "GIT_INDEX_FILE" (uiop:native-namestring temp)))))
      (uiop:delete-file-if-exists temp)
      (uiop:delete-file-if-exists (make-pathname :type (if (pathname-type temp)
                                                           (concatenate 'string (pathname-type temp) ".lock")
                                                           "lock")
                                                 :defaults temp)))))

(defun %index-tree (store)
  "Tree id of the real index (`write-tree` on a copy of it)."
  (%call-with-temp-index store t
                         (lambda (env) (%chomp (%git store '("write-tree") :extra-env env)))))

(defun %parent-state (store parent-ids)
  (if parent-ids
      (rev:revision-state store (first parent-ids))
      (diff:make-tree)))

(defun %content-tree (store content parent-ids)
  "Tree id for COMMIT-REVISION's CONTENT (see the comment at the top)."
  (typecase content
    (string
     (or (%rev-parse-verify store (concatenate 'string content "^{tree}"))
         (error 'rev:revision-error :message (format nil "~s names no tree" content))))
    ((eql :index) (%index-tree store))
    (diff:diff
     (%write-tree store (values (diff:apply-diff (%parent-state store parent-ids) content))))
    (rev:revision-payload
     (cond ((rev:payload-state-p content)
            (%content-tree store (rev:payload-state content) parent-ids))
           ((rev:payload-delta content)
            (%content-tree store (rev:payload-delta content) parent-ids))
           (t (error 'rev:revision-error :message "empty revision payload"))))
    (t
     (if (diff:tree-dir-p content)
         (%write-tree store content)
         (error 'rev:revision-error
                :message (format nil "cannot commit ~s: want a tree value, a tree id or revspec, :INDEX, a diff or a payload"
                                 content))))))

(defun %content-depends-on-parent-p (content)
  (or (typep content 'diff:diff)
      (and (typep content 'rev:revision-payload) (not (rev:payload-state-p content)))))

(defun %commit-tree (store tree parent-ids message provenance valid-at)
  "`commit-tree` → the new commit id."
  (let* ((now (%now-seconds))
         (author-at (if valid-at (dt:instant-seconds valid-at) now))
         (message (or message ""))
         (message (if (and (plusp (length message))
                           (char= #\Newline (char message (1- (length message)))))
                      message
                      (format nil "~a~%" message)))
         (env (append (list (cons "GIT_COMMITTER_DATE" (format nil "@~d +0000" now))
                            (cons "GIT_AUTHOR_DATE" (format nil "@~d +0000" author-at)))
                      (loop for (key var) in '((:author-name "GIT_AUTHOR_NAME")
                                               (:author-email "GIT_AUTHOR_EMAIL")
                                               (:committer-name "GIT_COMMITTER_NAME")
                                               (:committer-email "GIT_COMMITTER_EMAIL"))
                            for value = (getf provenance key)
                            when value collect (cons var (string value))))))
    (%chomp (%git store (append (list "commit-tree" tree)
                                (loop for p in parent-ids append (list "-p" p))
                                (list "-F" "-"))
                  :input message :extra-env env))))

(defmethod rev:commit-revision ((store git-store) content
                                &key (parents nil parents-p) ref (expected nil expected-p)
                                  message provenance valid-at id)
  (when id
    (error 'rev:revision-error :message "git chooses commit ids: :ID is not accepted"))
  (check-type ref (or null string))
  (check-type message (or null string))
  (check-type valid-at (or null dt:instant))
  (check-type parents list)
  (mapc #'%check-designator parents)
  (when expected (%check-designator expected))
  (when ref
    (let ((tag (%tag-target store ref)))
      (when tag (error 'rev:tag-immutable :name ref :target tag))))
  (let ((forcible (and ref (rev:permit-force-p store ref) t))
        (forced nil)
        (tree-cache nil))               ; (parent-ids . tree-id)
    (loop
      (let* ((tip (and ref (%branch-target store ref)))
             (parent-ids (if parents-p
                             (mapcar (lambda (d) (%resolve-id store d)) parents)
                             (and tip (list tip))))
             (tree (if (and tree-cache
                            (or (not (%content-depends-on-parent-p content))
                                (equal (car tree-cache) parent-ids)))
                       (cdr tree-cache)
                       (cdr (setf tree-cache
                                  (cons parent-ids (%content-tree store content parent-ids))))))
             (new (%commit-tree store tree parent-ids message provenance valid-at)))
        (unless ref
          (return (%commit store new)))
        (let ((want (if expected-p (and expected (%resolve-id store expected)) tip)))
          (multiple-value-bind (status current) (%cas-branch store ref new want (not forced))
            (ecase status
              (:unchanged (return (%commit store new)))
              (:ok
               (%notify store (list :ref-moved ref current new))
               (return (%commit store new)))
              (:conflict
               (restart-case (error 'rev:ref-conflict :name ref :expected want :actual current)
                 (rev:use-current ()
                   :report (lambda (s) (format s "Return the current tip of ~s; commit nothing." ref))
                   (return (and current (%commit store current))))
                 (rev:retry ()
                   :report (lambda (s)
                             (format s "Commit again with ~s's current target as expected." ref))
                   (setf expected current expected-p t))
                 (rev:force ()
                   :test (lambda (c) (declare (ignore c)) forcible)
                   :report (lambda (s) (format s "Commit onto ~s unconditionally." ref))
                   (setf forced t)))))))))))

;;; ------------------------------------------------------------------
;;; Graph
;;; ------------------------------------------------------------------

(defun %tips (store)
  "Commit ids of every branch, tag and HEAD: the graph set's roots."
  (remove-duplicates
   (append (mapcar #'rev:ref-target (rev:list-refs store))
           (let ((head (%rev-parse-verify store "HEAD^{commit}")))
             (and head (list head))))
   :test #'string=))

(defun %children-table (store)
  "Hash table id → child ids over the graph set, from `rev-list --children`."
  (let ((table (make-hash-table :test 'equal))
        (tips (%tips store)))
    (when tips
      (dolist (line (%lines (%git store '("rev-list" "--children" "--stdin")
                                  :input (format nil "~{~a~%~}" tips))))
        (let ((fields (remove "" (%split-string line #\Space) :test #'string=)))
          (when fields
            (setf (gethash (first fields) table) (rest fields))))))
    table))

(defun %commits (store ids)
  (mapcar (lambda (id) (%commit store id)) ids))

(defmethod rev:revision-parents ((store git-store) designator)
  (%check-designator designator)
  (%commits store (rev:revision-parent-ids (%resolve store designator))))

(defmethod rev:revision-children ((store git-store) designator)
  (%check-designator designator)
  (let ((id (%resolve-id store designator)))
    (%commits store (gethash id (%children-table store)))))

(defmethod rev:heads ((store git-store))
  (%commits store (loop for id being the hash-keys of (%children-table store)
                          using (hash-value children)
                        unless children collect id)))

(defun %rev-list (store options include exclude)
  "Ids from `rev-list OPTIONS INCLUDE… ^EXCLUDE…` (ids already resolved)."
  (when include
    (%lines (%git store (append (list "rev-list") options include
                                (mapcar (lambda (id) (concatenate 'string "^" id)) exclude))))))

(defun %footprints-overlap-p (footprints wanted)
  (some (lambda (f) (some (lambda (w) (diff:footprint-overlap-p f w)) wanted))
        footprints))

(defmethod rev:walk-revisions ((store git-store) from &key (order :topo) until reverse footprint)
  (check-type order (member :topo :time :first-parent))
  (check-type footprint list)
  (let* ((from-ids (%ids store from))
         (until-ids (%ids store until))
         (walk (%commits store
                         (%rev-list store
                                    (append (list (ecase order
                                                    (:topo "--topo-order")
                                                    (:time "--date-order")
                                                    (:first-parent "--first-parent")))
                                            (unless reverse (list "--reverse")))
                                    (if (eq order :first-parent)
                                        (and from-ids (list (first from-ids)))
                                        from-ids)
                                    until-ids))))
    (if footprint
        (remove-if-not (lambda (r)
                         (%footprints-overlap-p
                          (rev:delta-footprint (rev:revision-delta store r)) footprint))
                       walk)
        walk)))

(defmethod rev:revision-range ((store git-store) include exclude)
  (%commits store (%rev-list store '("--topo-order" "--reverse")
                             (%ids store include) (%ids store exclude))))

(defmethod rev:merge-base ((store git-store) a b &key all)
  (%check-designator a)
  (%check-designator b)
  (let* ((ia (%resolve-id store a))
         (ib (%resolve-id store b))
         (ids (%lines (%git store (append (list "merge-base") (when all (list "--all")) (list ia ib))
                            :ok '(0 1)))))
    (if all
        (%commits store ids)
        (and ids (%commit store (first ids))))))

(defmethod rev:ancestor-p ((store git-store) a b)
  (%check-designator a)
  (%check-designator b)
  (let ((ia (%resolve-id store a))
        (ib (%resolve-id store b)))
    (eql 0 (nth-value 1 (%git store (list "merge-base" "--is-ancestor" ia ib) :ok '(0 1))))))

;;; ------------------------------------------------------------------
;;; Content: states
;;; ------------------------------------------------------------------

(defmethod rev:content-primary ((store git-store))
  :state)

(defmethod rev:content-engine ((store git-store))
  (diff:find-diff-engine :tree))

(defmethod rev:ref-cas-p ((store git-store))
  "T: refs move by `git update-ref` with an expected old value (git's lock)."
  t)

(defmethod rev:conflicted-revisions-p ((store git-store))
  "NIL: a git commit records resolved trees only."
  nil)

(defun %classify (octets)
  "→ the text of OCTETS, or NIL when they are binary (a NUL in the first 8000
bytes, or not valid UTF-8)."
  (unless (find 0 octets :end (min 8000 (length octets)))
    (%utf8-decode-strict octets)))

(defun %blob-file (store id mode)
  "File node for the blob ID with MODE."
  (multiple-value-bind (type octets) (%batch-object store "contents" id)
    (unless (equal type "blob")
      (error 'git-command-error :command (list "cat-file" "blob" id) :exit-code nil
                                :stderr (format nil "~a is not a blob" id)))
    (let ((text (%classify octets)))
      (if text
          (diff:make-tree-file text :type :text :mode mode)
          (diff:make-tree-file (%octets octets) :type :binary :mode mode)))))

(defun %build-tree (pairs)
  "Tree value from PAIRS ((names . file-node) …)."
  (let ((order '())
        (table (make-hash-table :test 'equal)))
    (dolist (p pairs)
      (let ((name (first (car p))))
        (unless (nth-value 1 (gethash name table))
          (push name order)
          (setf (gethash name table) '()))
        (if (rest (car p))
            (push (cons (rest (car p)) (cdr p)) (gethash name table))
            (setf (gethash name table) (cdr p)))))
    (apply #'diff:make-tree
           (loop for name in (nreverse order)
                 for value = (gethash name table)
                 collect name
                 collect (if (diff:tree-file-p value)
                             value
                             (%build-tree (reverse value)))))))

(defun %tree-state (store tree-id)
  "Tree value of the tree object TREE-ID."
  (%build-tree
   (loop for e in (parse-ls-tree (%git store (list "ls-tree" "-r" "-z" "-l" tree-id)))
         collect (cons (%split-string (getf e :path) #\/)
                       (if (eq (getf e :type) :commit)
                           (diff:make-tree-file (getf e :id) :type :gitlink :mode #o160000)
                           (%blob-file store (getf e :id) (getf e :mode)))))))

(defmethod rev:revision-state ((store git-store) designator)
  (%check-designator designator)
  (%tree-state store (rev:revision-content (%resolve store designator))))

(defgeneric %operand-tree (store operand)
  (:documentation "Tree id STATE-DIFF compares for OPERAND."))

(defmethod %operand-tree ((store git-store) operand)
  (%check-designator operand)
  (rev:revision-content (%resolve store operand)))

(defmethod rev:revision-delta ((store git-store) designator)
  (%check-designator designator)
  (let* ((r (%resolve store designator))
         (parent (%first-parent store r)))
    (%diff-trees store (if parent (rev:revision-content parent) (%empty-tree store))
                 (rev:revision-content r) :changes nil)))

(defmethod rev:state-diff ((store git-store) a b &key footprint (level :changes))
  (check-type footprint list)
  (check-type level (member :summary :changes :content))
  (%diff-trees store (%operand-tree store a) (%operand-tree store b) level footprint))

;;; ------------------------------------------------------------------
;;; Content: diffs between two tree ids
;;; ------------------------------------------------------------------

(defun %tree-fp (names)
  (format nil "tree:/~{~a~^/~}" names))

(defun %path-names (path)
  "The leading strings of a change path: the file part."
  (loop for step in path while (stringp step) collect step))

(defun %tree-at (names node)
  "Tree value holding only NODE at NAMES."
  (diff:make-tree (first names) (if (rest names) (%tree-at (rest names) node) node)))

(defun %name-status-entries (store ta tb)
  "→ list of (kind names from-names), git's file-level changes with -M."
  (loop for e in (parse-name-status
                  (%git store (list "diff-tree" "-r" "-z" "-M" "--name-status" "--no-commit-id" ta tb)))
        collect (list (ecase (getf e :status)
                        ((:modify :type-change :unmerged) :modify)
                        ((:add :copy) :add)
                        (:remove :remove)
                        (:move :move))
                      (%split-string (getf e :path) #\/)
                      (and (eq (getf e :status) :move)
                           (%split-string (getf e :from) #\/)))))

(defun %file-change (kind names from &key before-fn after-fn (before nil before-p) (after nil after-p))
  (apply #'diff:make-change kind :path names :from from
         :footprint (if from (list (%tree-fp from) (%tree-fp names)) (list (%tree-fp names)))
         (append (unless (eq kind :add)
                   (if before-p (list :before before) (list :before-fn before-fn)))
                 (unless (eq kind :remove)
                   (if after-p (list :after after) (list :after-fn after-fn))))))

(defun %plain-text-p (node)
  (and (eq :text (diff:tree-file-type node))
       (stringp (diff:tree-file-content node))
       (/= #o160000 (or (diff:tree-file-mode node) #o100644))))

(defun %link-p (node)
  (eql #o120000 (diff:tree-file-mode node)))

(defun %group-applies-p (engine names from before after group)
  "True when the parsed changes GROUP of one file take BEFORE to AFTER and
back (the law-3 check on a tree holding only that file)."
  (and group
       (every #'%plain-text-p (remove nil (list before after)))
       (or (null before) (null after) (eq (%link-p before) (%link-p after)))
       (let ((ma (if before (%tree-at (or from names) before) (diff:make-tree)))
             (mb (if after (%tree-at names after) (diff:make-tree)))
             (d (diff:make-diff group :engine engine :level :changes)))
         (handler-case
             (and (diff:state-equal engine (values (diff:apply-diff ma d)) mb)
                  (diff:state-equal engine (values (diff:apply-diff mb (diff:invert-diff d))) ma))
           (diff:diff-error () nil)))))

(defun %parsed-patch (store ta tb)
  "Top-level changes of `diff-tree -p -M` between TA and TB through
DIFF-PROTOCOL:PARSE-DIFF :UNIFIED (NIL when git printed something it cannot parse)."
  (let ((patch (%utf8-decode
                (%git store (list "diff-tree" "-r" "-p" "-M" "--no-color" "--no-ext-diff"
                                  "--no-textconv" "--full-index" "--no-commit-id"
                                  "--src-prefix=a/" "--dst-prefix=b/" ta tb)
                      :output :octets))))
    (handler-case (diff:diff-changes (diff:parse-diff :unified patch))
      (diff:diff-parse-error () '()))))

(defun %change-rank (c)
  (if (notevery #'stringp (diff:change-path c))
      2
      (ecase (diff:change-kind c) (:remove 0) (:move 1) (:modify 2) (:add 3))))

(defun %diff-trees (store ta tb level footprint)
  "DIFF from tree TA to tree TB at LEVEL (see STATE-DIFF in the comment at the top)."
  (let ((engine (rev:content-engine store)))
    (if (string= ta tb)
        (diff:make-diff '() :engine engine :level level)
        (let* ((sa nil) (sb nil)
               (entries (%name-status-entries store ta tb))
               (changes '()))
          (flet ((state-a () (or sa (setf sa (%tree-state store ta))))
                 (state-b () (or sb (setf sb (%tree-state store tb)))))
            (if (eq level :summary)
                (dolist (e entries)
                  (destructuring-bind (kind names from) e
                    (let ((before-names (or from names)))
                      (push (%file-change kind names from
                                          :before-fn (lambda () (values (diff:tree-get (state-a) before-names)))
                                          :after-fn (lambda () (values (diff:tree-get (state-b) names))))
                            changes))))
                (let ((parsed (%parsed-patch store ta tb)))
                  (dolist (e entries)
                    (destructuring-bind (kind names from) e
                      (let* ((before (unless (eq kind :add) (values (diff:tree-get (state-a) (or from names)))))
                             (after (unless (eq kind :remove) (values (diff:tree-get (state-b) names))))
                             (group (remove-if-not (lambda (c) (equal names (%path-names (diff:change-path c))))
                                                   parsed))
                             (kept (%group-applies-p engine names from before after group))
                             (whole (%file-change kind names from :before before :after after)))
                        (cond ((not kept) (push whole changes))
                              ((eq level :changes) (dolist (c group) (push c changes)))
                              ((eq kind :modify)
                               (push (diff:make-change :modify :path names :footprint (list (%tree-fp names))
                                                               :before before :after after :children group)
                                     changes))
                              (t (push whole changes)))))))))
          (setf changes (stable-sort (nreverse changes) #'< :key #'%change-rank))
          (when footprint
            (setf changes (remove-if-not (lambda (c) (%footprints-overlap-p (diff:change-footprint c) footprint))
                                         changes)))
          (diff:make-diff changes :engine engine :level level)))))
