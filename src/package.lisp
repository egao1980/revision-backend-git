(defpackage #:revision-backend-git
  (:use #:cl)
  (:nicknames #:stack-rev-git)
  (:local-nicknames (#:rev #:revision-protocol)
                    (#:diff #:diff-protocol)
                    (#:dt #:datetime-protocol)
                    (#:proc #:process-protocol)
                    (#:pathlib #:cl-stack-pathlib))
  ;; revision-protocol's condition (shared with revision-backend-jj), re-exported.
  (:import-from #:revision-protocol
                #:vc-tool-version-error
                #:vc-tool-version-error-tool
                #:vc-tool-version-error-found
                #:vc-tool-version-error-required)
  (:export
   ;; conditions: VC-TOOL-VERSION-ERROR is revision-protocol's (re-exported);
   ;; GIT-COMMAND-ERROR is git-specific; everything else is a protocol condition.
   #:vc-tool-version-error
   #:vc-tool-version-error-tool
   #:vc-tool-version-error-found
   #:vc-tool-version-error-required
   #:git-command-error
   #:git-command-error-command
   #:git-command-error-exit-code
   #:git-command-error-stderr

   ;; git program and version
   #:*git-program*
   #:*minimum-git-version*
   #:parse-git-version
   #:git-version

   ;; store
   #:git-store
   #:git-store-p
   #:make-git-store
   #:git-store-path
   #:git-store-git-dir
   #:git-store-bare-p
   #:git-store-env
   #:run-git
   #:git-store-batch-active-p
   #:close-git-store
   #:with-git-store

   ;; working copy (a revision-protocol target)
   #:git-working-copy
   #:git-working-copy-p
   #:make-git-working-copy
   #:working-copy-store
   #:working-copy-path
   #:working-copy-stamp-ref

   ;; read-only revision filesystem (cl-stack-pathlib)
   #:revision-filesystem
   #:revision-filesystem-p
   #:make-revision-filesystem
   #:revision-filesystem-store
   #:revision-filesystem-revision
   #:revision-path

   ;; parsers of machine git output (pure; fuzzed from corpora)
   #:parse-status-porcelain-v2
   #:parse-name-status
   #:parse-ls-tree

   ;; ---- V1 -------------------------------------------------------------
   ;; projections (revision-protocol GFs on GIT-STORE / GIT-PROJECTION)
   #:git-projection
   #:git-projection-p
   #:git-projection-tree
   #:git-projection-worktree
   #:merge-projection-marker-tree
   #:*projection-directory*
   #:make-tree-filesystem
   #:revision-filesystem-tree
   #:apply-patch
   #:patch-rejected
   #:patch-rejected-projection
   #:patch-rejected-stderr

   ;; linked worktrees
   #:git-store-common-dir
   #:add-worktree
   #:list-worktrees
   #:remove-worktree
   #:parse-worktree-list

   ;; replay
   #:replay-revisions
   #:replay-conflict
   #:replay-conflict-revision
   #:replay-conflict-onto))
