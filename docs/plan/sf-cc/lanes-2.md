# sf-cc Generation 2 Lane Manifest

- Lanes status: draft
- Plan: plan-2.md
- Concurrency: lanes
- Merge policy: merge-ready FIFO
- Merge concurrency: 1

## Control worktree

- Name: async-sf-cc
- Path: /home/slepher/project/erl_projects/async-sf-cc

## Defaults

- Default coding lane: coding
- Default integration lane: integrate

## Lane coding

- Worktree name: coding
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-cc-generation-2-coding
- Serial: true

## Lane integrate

- Worktree name: integrate
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-cc-generation-2-integrate
- Serial: true

## Phase task-coding-2

- Kind: coding
- Mode: hard
- Mode basis: `async_sf_cc`、`async_t`、`async_r_t`、`async_util`、`async_sf_runtime` 与 `async_sf_transform` 必须同时切换同一 ContinuationRef lifecycle；exactly-once take、direct-parent forwarding、reverse ownership index、callback exception classification 是跨模块 shared invariant，隔离 handoff 会产生不可接受的双表示或双 owner。
- Lane: coding
- Base: 0adf22a22f20c45fff192d52398f23f83bf4548c
- Depends on: none
- Covers: none

## Phase task-test-2

- Kind: test
- Mode: hard
- Mode basis: R1–R19 与 sf-do linked contract 需要在同一 public end-to-end observation matrix 中关联 runner、action lifecycle、generated frame、shared parent、store backend 与 exactly-once并发结果；拆分验证会丢失跨边界因果与同一 acceptance gate。
- Lane: integrate
- Base: task-coding-2 accepted merge-ready candidate atop 0adf22a22f20c45fff192d52398f23f83bf4548c
- Depends on: none
- Covers: task-coding-2
