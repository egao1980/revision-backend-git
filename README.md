# revision-backend-git

[revision-protocol](https://github.com/egao1980/revision-protocol) backend over the **git CLI** (no libgit2 / gitoxide), for [cl-stack](https://github.com/egao1980/cl-stack) (`stack-rev-git`).

Capability brief: [`cl-stack/docs/capabilities/revision-backend-git.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/revision-backend-git.md) · plan: [HARNESS-PLAN.md](https://github.com/egao1980/cl-workspace/blob/main/docs/HARNESS-PLAN.md) Track V.

Version **0.2.0** — milestone V0 on revision-protocol 0.3.0 / diff-protocol 0.2.1 (C-quoted paths — TAB, newline, `"`, `\` — now get hunks at `:changes` / `:content`; `capture-revision` takes `:valid-at` / `:provenance`; `ref-cas-p` T, `conflicted-revisions-p` NIL). V0: a [revision-protocol](https://github.com/egao1980/revision-protocol) store, working copy and read-only revision filesystem over the **git CLI** (minimum git 2.43.0; no libgit2). Projections, `commit-projection` and `execute-plan` are V1. Requires process-protocol 0.3.0 / process-backend-uiop 0.2.0 (octet capture, env merge, octet streams, timeouts).

## V0 surface

- **`git-store`** (a `revision-store`): `make-git-store path &key git env permit-force`. Revisions are commits (full hash ids; parents in order; recorded-at = committer date, valid-at = author date; content = tree id). Refs: `refs/heads/*` movable, `refs/tags/*` tags; CAS through `update-ref <ref> <new> <old>`. Graph walks through `rev-list`, `merge-base`, `merge-base --is-ancestor`. Content is `:state`: tree values of [diff-protocol](https://github.com/egao1980/diff-protocol); `state-diff` from `diff-tree -z -M --name-status` (`:summary`) and `git diff` → `parse-diff :unified` (`:changes`, `:content`), binary files as whole-file changes.
- **`git-working-copy`** (a `revision-target`): stamp = HEAD or a private ref (`:stamp-ref "refs/checkpoints/x"`); `target-state` = what `git add -A` would record; `target-drift`; `capture-revision` through a temporary index (worktree and real index untouched).
- **`revision-filesystem`**: a read-only [cl-stack-pathlib](https://github.com/egao1980/cl-stack-pathlib) filesystem over one commit (`ls-tree` / `cat-file --batch-command`), so pathlib tools read any revision: `(revision-path store "main" "/src/x.lisp")`.
- **`run-git`**, parsers `parse-status-porcelain-v2`, `parse-name-status`, `parse-ls-tree`.
- **Conditions:** the protocol's own (`revision-not-found`, `ambiguous-revision`, `ref-conflict`, `tag-immutable`, `revision-error`) plus `vc-tool-version-error` (tool / found / required; revision-protocol's symbol, re-exported) and `git-command-error` (command / exit-code / stderr).

Every git command runs with `LC_ALL=C GIT_TERMINAL_PROMPT=0 GIT_CONFIG_NOSYSTEM=1 GIT_OPTIONAL_LOCKS=0`, `-c core.quotepath=off`, and `-z --no-color --no-ext-diff` where git accepts them.

**git ≥ 2.43.0** (`*minimum-git-version*`): the oldest git on the CI runners (the cl-repository `ci-base` container, Ubuntu 24.04). Older git signals `vc-tool-version-error` from `make-git-store`.

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
