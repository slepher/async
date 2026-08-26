# sf-cc Lane Manifest

- Lanes status: accepted
- Plan: plan-1.md
- Concurrency: lanes
- Merge policy: merge-ready FIFO
- Merge concurrency: 1

## Control worktree

- Name: async-sf-cc
- Path: /home/slepher/project/erl_projects/async-sf-cc

## Lane coding

- Worktree name: coding
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-cc-g1-coding
- Serial: true

## Lane integrate

- Worktree name: integrate
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-cc-g1-integrate
- Serial: true

## Defaults

- Default coding lane: coding
- Default integration lane: integrate

## Phase task-coding-1

- Kind: coding
- Mode: hard
- Mode basis: callback representation、reader scope、runner normalization、cleanup-before-invoke、frame fallback 与 old-code compatibility 共同维护跨模块 exactly-once lifecycle；隔离交接会丢失同一 State/CcRef invariant
- Lane: coding
- Depends on: none
- Covers: none
- Base: 9691772e7e260e32cb5c7fd11c0cd51563b3530a

## Phase task-test-1

- Kind: test
- Mode: hard
- Mode basis: compiler macro、三个 runtime mode、旧升级 fixture、并发 lifecycle、frame metadata、matcher atomicity 与多 backend 必须在同一不可变 candidate 上联合验证兼容性
- Lane: integrate
- Depends on: none
- Covers: task-coding-1
- Base: task-coding-1 accepted merge on control target

## Phase task-test-1-fix-1

- Kind: coding
- Mode: hard
- Mode basis: inherited from task-coding-1
- Lane: coding
- Depends on: task-test-1, task-coding-1
- Covers: none
- Triggered by: task-test-1
- Affected coding task: task-coding-1
- Failure observed target: bc28febe142000025abaf59c71c8b41ab6cffe99
- Integration checkpoint: bc28febe142000025abaf59c71c8b41ab6cffe99
- Fix base: bc28febe142000025abaf59c71c8b41ab6cffe99
