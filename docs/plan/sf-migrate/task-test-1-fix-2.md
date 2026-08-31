# Coding Fix Contract

- Task: task-test-1-fix-2
- Parent finding: task-test-1.1
- Bundle generation: 1
- Task kind: coding
- Mode: normal
- Execution strategy: single-def
- Worker role: def_coding_worker
- Lane: coding
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-migrate-g1-coding
- Base: 93e0c3c6d39edf7a1c5ef5438ffbb388afbee186
- Target branch: async-sf-cc
- OpenSpec authority: openspec/changes/sf-migrate/

## Finding

On immutable integrate candidate `c249feb`, compiling one FrameOwner with two
exact historical migration declarations fails with:

`{redefine_function,{'$async_sf_manifest',0}}`

The accepted contract requires multiple exact declarations in one ordinary
`sf_do_code_change/2` router.

## Bounded goal

修复 parse-transform generated-form assembly，使一个 owner 含多个
`sf_migrate/3` declarations 时只生成一个 current manifest，同时保留所有
historical plans and exact router behavior. Preserve single-declaration output,
runtime behavior, diagnostic behavior, and current manifest shape.

## Owned paths

- `src/async_sf_transform.erl`

Tests, README, OpenSpec, workflow files, and unrelated source are read-only.

## Implementation simplicity

- Standard: `audit-implementation-simplicity`
- Reuse the existing `finish_rewrite/3` and manifest generation owner.
- Remove only the duplicate generated-form path; do not add registries, graph composition, or new manifest abstractions.
- Preserve one manifest owner and existing plan aggregation.

## Required work and validation

1. Trace why multiple migration declarations produce duplicate manifest forms.
2. Make the smallest source-only correction preserving all plans and generated bridges.
3. Run `rebar3 compile`, `rebar3 eunit`, `rebar3 xref`, and `git diff --check`.
4. Use a real compile probe for one and two historical declarations; do not edit tests.

## Done when

- The two-declaration fixture compiles and both exact router branches return plans.
- Existing compile, EUnit, xref, and diff checks pass.
- Only the owned source path changes.

## Handoff

Return exact Status, Task, Result, Evidence, Changes, Validation, and Caveats.
Do not stage, commit, merge, or modify tests. Dispatcher routes the reviewed
checkpoint to target, refreshes integrate, and reruns the affected test.

## Stop conditions

- The fix requires changing accepted migration behavior outside generated-form assembly.
- A test/OpenSpec/workflow/unrelated-source edit is needed.
- Any blocking check fails outside the owned path.
