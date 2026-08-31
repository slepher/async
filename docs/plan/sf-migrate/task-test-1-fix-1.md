# Coding Fix Contract

- Task: task-test-1-fix-1
- Parent finding: task-test-1.1
- Bundle generation: 1
- Task kind: coding
- Mode: normal
- Execution strategy: single-def
- Worker role: def_coding_worker
- Lane: coding
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-migrate-g1-coding
- Base: 37093ef1f9820dfc88f7d13c038b55f6e7530880
- Target branch: async-sf-cc
- OpenSpec authority: openspec/changes/sf-migrate/

## Finding

On immutable integrate candidate `b88b1076b648f44b6615b612e9b13d1e10f2faf1`,
`rebar3 ct --suite=test/async_sf_migrate_SUITE.erl` fails because compiling a
module containing `sf_migrate/3` without explicit `-vsn` returns `{error, [], []}`
instead of a public compiler diagnostic containing
`sf_migrate_requires_explicit_vsn`.

## Bounded goal

修复 compiler diagnostic materialization，使 sf-migrate source errors retain
their source file and are returned by `compile:file/2`, including
`sf_migrate_requires_explicit_vsn` and `invalid_sf_migrate_syntax`. Preserve
successful migration compilation, existing diagnostics, and all runtime
behavior. Do not alter the accepted OpenSpec contract.

## Owned paths

- `src/async_sf_transform.erl`

All tests, README, OpenSpec, workflow files, and unrelated source are read-only.

## Implementation simplicity

- Standard: `audit-implementation-simplicity`
- Reuse the existing Astranaut error/return pipeline; extend the one parse-transform boundary that loses file-bound diagnostics.
- Do not add a diagnostic abstraction, new error registry, or test-only product path.
- Keep the change limited to the shared source-error boundary so all compiler source errors benefit consistently.

## Required work and validation

1. Trace the existing parse-transform return/error path and correct only the
   source-file binding needed for formatted diagnostics.
2. Verify exact public diagnostic reasons without changing their names or
   payloads.
3. Run `rebar3 compile`, `rebar3 eunit`, `rebar3 xref`, and `git diff --check`.
4. Do not stage, commit, merge, modify tests, or repair unrelated failures.

## Done when

- The two cited compiler diagnostics are returned with non-empty errors.
- Existing compile, EUnit, and xref checks pass.
- Only the owned source path changes and the worker reports exact evidence.

## Handoff

Return `Status`, `Task`, `Result`, `Evidence`, `Changes`, `Validation`, and
`Caveats`. The dispatcher will checkpoint and review the coding fix, merge it to
target, refresh integrate, then rerun the affected and full test gates.

## Stop conditions

- The fix requires changing behavior outside compiler diagnostic materialization.
- A test, OpenSpec, workflow, or unrelated source edit is needed.
- Any blocking check fails outside this owned path.
