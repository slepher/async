# sf-migrate generation-1 lane manifest

- Lanes status: complete
- Plan: plan-1.md
- Concurrency: lanes
- Merge policy: merge-ready FIFO
- Merge concurrency: 1

## Control worktree

- Name: async-sf-cc
- Path: /home/slepher/project/erl_projects/async-sf-cc
- Target branch: async-sf-cc
- Target base: aa7c6cd4f6b79659b1293b753a7aa5dd2f98cf41

## Lane coding

- Worktree name: coding
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-migrate-g1-coding
- Serial: true

## Lane integrate

- Worktree name: integrate
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-migrate-g1-integrate
- Serial: true

## Defaults

- Default coding lane: coding
- Default integration lane: integrate
- Route: coding -> target -> integrate -> target
- Product correction route: task-test-1 finding -> task-test-1-fix-N in coding -> target -> integrate refresh/rerun -> target

## Phase task-coding-1

- Kind: coding
- Mode: hard
- Mode basis: historical AST analysis, frozen metadata, generated bridge identity, and runtime plan validation form one compiler/runtime representation invariant; splitting them would create an unusable half-contract
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Lane: coding
- Depends on: none
- Covers: none
- Base: aa7c6cd4f6b79659b1293b753a7aa5dd2f98cf41

## Phase task-coding-2

- Kind: coding
- Mode: hard
- Mode basis: converter invocation, generated target validators, explicit precedence, designed failures, and user exception propagation share one bridge/runtime semantic boundary
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Lane: coding
- Depends on: task-coding-1
- Covers: none
- Base: task-coding-1 accepted merge on target branch async-sf-cc

## Phase task-coding-3

- Kind: coding
- Mode: normal
- Mode basis: the down behavior is intentionally reuse of the actual current manifest and existing automatic path; only a narrow runtime closure plus two README surfaces remain after task-coding-2
- Execution strategy: single-def
- Worker role: def_coding_worker
- Lane: coding
- Depends on: task-coding-2
- Covers: none
- Base: task-coding-2 accepted merge on target branch async-sf-cc

## Phase task-test-1

- Kind: test
- Mode: normal
- Mode basis: 一个边界明确的 public black-box test-asset subtask 可独立写入并 focused-run；之后保持不变的完整 repository/OTP 验证在其不可变 checkpoint 上只读执行
- Execution strategy: split-def
- Worker role: def_coding_worker
- Subtasks: task-test-1.1
- Parent verification role: full_tester
- Lane: integrate
- Depends on: none
- Covers: task-coding-1, task-coding-2, task-coding-3
- Base: task-coding-3 accepted merge on target branch async-sf-cc

## Phase task-test-1-fix-1

- Kind: coding
- Mode: normal
- Mode basis: the test finding is localized to compiler diagnostic materialization and has one source owner with a deterministic regression check
- Execution strategy: single-def
- Worker role: def_coding_worker
- Lane: coding
- Depends on: task-test-1
- Covers: none
- Base: 37093ef1f9820dfc88f7d13c038b55f6e7530880
- Finding: task-test-1.1 on b88b1076b648f44b6615b612e9b13d1e10f2faf1 receives `{error, [], []}` instead of a diagnostic containing `sf_migrate_requires_explicit_vsn` for a migration-enabled module without explicit `-vsn`

## Phase task-test-1-fix-2

- Kind: coding
- Mode: normal
- Mode basis: the finding is localized to one generated manifest boundary and has a deterministic multi-declaration compiler reproduction
- Execution strategy: single-def
- Worker role: def_coding_worker
- Lane: coding
- Depends on: task-test-1-fix-1
- Covers: none
- Base: 93e0c3c6d39edf7a1c5ef5438ffbb388afbee186
- Finding: task-test-1.1 on c249feb generates duplicate `'$async_sf_manifest'/0` when one owner declares multiple exact historical migrations

## Visibility and closure gates

- A phase base is resolved only after the named predecessor is reviewed and merged to target; lane-local candidate visibility is insufficient.
- `Covers` means `task-test-1` cannot begin until all three coding results are present on target and integrate is based on that target commit.
- Test-owned changes return from integrate to target. Product fixes never merge directly from coding to integrate.
- No phase provisions its own lane, accepts its own result, stages/commits/merges, or updates workflow status.
