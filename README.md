# revision-backend-git

[revision-protocol](https://github.com/egao1980/revision-protocol) backend over the **git CLI** (no libgit2 / gitoxide), for [cl-stack](https://github.com/egao1980/cl-stack) (`stack-rev-git`).

Capability brief: [`cl-stack/docs/capabilities/revision-backend-git.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/revision-backend-git.md) · plan: [HARNESS-PLAN.md](https://github.com/egao1980/cl-workspace/blob/main/docs/HARNESS-PLAN.md) Track V.

Version **0.5.1** — loads with cl-stack-pathlib **0.7.0**: the read-only revision filesystem accepts `fs-write-atomic :mode` and refuses `fs-chmod` with `unsupported-operation` (#10). 0.5.0 — revision-protocol 0.4.0: ref names starting with `refs/` are used as given (CAS on custom namespaces such as `refs/cl-stack-code/plans/<id>`, `refs/tags/<name>` is a tag; `list-refs :prefix "refs/…"` lists full names), short names keep `refs/heads/` (#8); `commit-revision :expected` without `:ref` signals `revision-error`; `tag-rewrite-p` is T and `tag-revision` offers `replace-annotation` on a same-target tag (new tag object via `mktag`, moved with `update-ref NEW OLD`). 0.4.0 — minimum git **2.56.0**, `replay-revisions` unconditional (`*replay-git-version*` / `git-replay-p` removed); working-copy drift checks keep a per-worktree stat index (`<git dir>/rbg-stat-index`) under git's racy-git rule, so only changed files are hashed, and tree writes touch only the paths that differ. 0.3.0 — milestone **V1**: projections (virtual trees and materialized linked worktrees), `project-merge` via `merge-tree --write-tree`, `commit-projection` with conflict restarts, `apply-patch` in a temp index, `plan-transition` / `execute-plan` / `project-plan` (`:exact`, `:shadow`), linked worktrees, `replay-revisions`. Brief: [revision-backend-git.md § V1](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/revision-backend-git.md#v1--locked). V0 (0.2.x): a [revision-protocol](https://github.com/egao1980/revision-protocol) store, working copy and read-only revision filesystem over the **git CLI** (no libgit2); revision-filesystem globs go through cl-stack-pathlib 0.6.1's generic `fs-glob`. Requires process-protocol 0.3.0 / process-backend-uiop 0.2.0.

## V0 surface

- **`git-store`** (a `revision-store`): `make-git-store path &key git env permit-force`. Revisions are commits (full hash ids; parents in order; recorded-at = committer date, valid-at = author date; content = tree id). Refs: `refs/heads/*` movable, `refs/tags/*` tags; CAS through `update-ref <ref> <new> <old>`. Graph walks through `rev-list`, `merge-base`, `merge-base --is-ancestor`. Content is `:state`: tree values of [diff-protocol](https://github.com/egao1980/diff-protocol); `state-diff` from `diff-tree -z -M --name-status` (`:summary`) and `git diff` → `parse-diff :unified` (`:changes`, `:content`), binary files as whole-file changes.
- **`git-working-copy`** (a `revision-target`): stamp = HEAD or a private ref (`:stamp-ref "refs/checkpoints/x"`); `target-state` = what `git add -A` would record; `target-drift`; `capture-revision` through a temporary index (worktree and real index untouched).
- **`revision-filesystem`**: a read-only [cl-stack-pathlib](https://github.com/egao1980/cl-stack-pathlib) filesystem over one commit (`ls-tree` / `cat-file --batch-command`), so pathlib tools read any revision: `(revision-path store "main" "/src/x.lisp")`.
- **`run-git`**, parsers `parse-status-porcelain-v2`, `parse-name-status`, `parse-ls-tree`.
- **Conditions:** the protocol's own (`revision-not-found`, `ambiguous-revision`, `ref-conflict`, `tag-immutable`, `revision-error`) plus `vc-tool-version-error` (tool / found / required; revision-protocol's symbol, re-exported) and `git-command-error` (command / exit-code / stderr).

Every git command runs with `LC_ALL=C GIT_TERMINAL_PROMPT=0 GIT_CONFIG_NOSYSTEM=1 GIT_OPTIONAL_LOCKS=0`, `-c core.quotepath=off`, and `-z --no-color --no-ext-diff` where git accepts them.

**git ≥ 2.56.0** (`*minimum-git-version*`): the oldest git on the CI runners (probed 2026-11-20 by the `git-version` workflow: ci-base 2.56.1, ubuntu-latest 2.56.1, macos-latest 2.56.0, windows-latest 2.56.1.windows.1). Older git signals `vc-tool-version-error` from `make-git-store`.

```lisp
(stack-rev-git:with-git-store (s #p"/path/to/repo/")
  (stack-rev:walk-revisions s (stack-rev:rev-ref "main") :order :first-parent)
  (stack-rev:state-diff s "main~1" "main" :level :summary))
```

## Tests

```sh
cl-repo test --system revision-backend-git
```

## License

MIT
