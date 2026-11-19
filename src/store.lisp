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
;;;   characters, '"', '\') are supported at :SUMMARY only in V0.
;;; ------------------------------------------------------------------

(defmethod rev:resolve-revision ((store git-store) designator)
  (declare (ignore designator))
  (%not-implemented 'rev:resolve-revision))

(defmethod rev:commit-revision ((store git-store) content
                                &key parents ref expected message provenance
                                  valid-at id)
  (declare (ignore content parents ref expected message provenance valid-at id))
  (%not-implemented 'rev:commit-revision))

(defmethod rev:move-ref ((store git-store) name designator &key expected)
  (declare (ignore name designator expected))
  (%not-implemented 'rev:move-ref))

(defmethod rev:tag-revision ((store git-store) name designator &key annotation)
  (declare (ignore name designator annotation))
  (%not-implemented 'rev:tag-revision))

(defmethod rev:delete-ref ((store git-store) name &key expected)
  (declare (ignore name expected))
  (%not-implemented 'rev:delete-ref))

(defmethod rev:find-ref ((store git-store) name)
  (declare (ignore name))
  (%not-implemented 'rev:find-ref))

(defmethod rev:list-refs ((store git-store) &key kind prefix)
  (declare (ignore kind prefix))
  (%not-implemented 'rev:list-refs))

(defmethod rev:revision-parents ((store git-store) designator)
  (declare (ignore designator))
  (%not-implemented 'rev:revision-parents))

(defmethod rev:revision-children ((store git-store) designator)
  (declare (ignore designator))
  (%not-implemented 'rev:revision-children))

(defmethod rev:walk-revisions ((store git-store) from
                               &key (order :topo) until reverse footprint)
  (declare (ignore from order until reverse footprint))
  (%not-implemented 'rev:walk-revisions))

(defmethod rev:merge-base ((store git-store) a b &key all)
  (declare (ignore a b all))
  (%not-implemented 'rev:merge-base))

(defmethod rev:ancestor-p ((store git-store) a b)
  (declare (ignore a b))
  (%not-implemented 'rev:ancestor-p))

(defmethod rev:revision-range ((store git-store) include exclude)
  (declare (ignore include exclude))
  (%not-implemented 'rev:revision-range))

(defmethod rev:heads ((store git-store))
  (%not-implemented 'rev:heads))

(defmethod rev:ref-listener ((store git-store))
  (%not-implemented 'rev:ref-listener))

(defmethod (setf rev:ref-listener) (function (store git-store))
  (declare (ignore function))
  (%not-implemented '(setf rev:ref-listener)))

(defmethod rev:permit-force-p ((store git-store) name)
  (declare (ignore name))
  (%not-implemented 'rev:permit-force-p))

(defmethod rev:content-primary ((store git-store))
  (%not-implemented 'rev:content-primary))

(defmethod rev:content-engine ((store git-store))
  (%not-implemented 'rev:content-engine))

(defmethod rev:revision-state ((store git-store) designator)
  (declare (ignore designator))
  (%not-implemented 'rev:revision-state))

(defmethod rev:revision-delta ((store git-store) designator)
  (declare (ignore designator))
  (%not-implemented 'rev:revision-delta))

(defmethod rev:state-diff ((store git-store) a b &key footprint (level :changes))
  (declare (ignore a b footprint level))
  (%not-implemented 'rev:state-diff))
