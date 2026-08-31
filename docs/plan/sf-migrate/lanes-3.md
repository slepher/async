# sf-migrate 第 3 代 lane manifest

- Lanes status: accepted
- Plan: plan-3.md
- Concurrency: lanes
- Merge policy: merge-ready FIFO
- Merge concurrency: 1

## Control worktree

- Name: async-sf-cc
- Path: /home/slepher/project/erl_projects/async-sf-cc
- Target branch: async-sf-cc
- Target base: 7552a0c7d6bd1cd167b9822534ad7327352e1f81

## Lane coding

- Worktree name: coding
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-migrate-g3-coding
- Serial: true

## Lane integrate

- Worktree name: integrate
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-migrate-g3-integrate
- Serial: true

## Defaults

- Default coding lane: coding
- Default integration lane: integrate
- Route: coding -> target -> integrate -> target
- Project bootstrap: 遵循仓库说明和现有 rebar setup；通用 lane manifest 不引入项目专用 bootstrap 命令
- Product correction route: task-test-3 发现 -> coding 中的 task-test-3-fix-N -> target -> integrate 刷新/重跑 -> target

## Phase task-test-3

- Kind: test
- Mode: hard
- Mode basis: 一个共享的 release/application/node/controller lifecycle 覆盖全部 fixture 版本和场景；需要连续的跨版本与分布式 node 推理
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Parent verification role: full_tester
- Lane: integrate
- Depends on: none
- Covers: none
- Base: 7552a0c7d6bd1cd167b9822534ad7327352e1f81

## Ownership and visibility gates

- `task-test-3` 独占 `test/app_fixtures/**`、`test/test_async_controller.erl` 和 `test/async_sf_migrate_application_SUITE.erl`。
- 对 worker 而言，生产代码、现有测试、文档/规范、配置、workflow artifact 和 `test.md` 均为只读。
- dispatch 前必须将 integrate 干净地重新绑定到精确 target base。现有第 1/2 代 lane binding 属于历史记录，不得复用为执行身份。
- 产品修正只有在已 review 的 coding candidate 到达 target，且 integrate 从该精确 target HEAD 刷新后才可见。lane-local 或直接 coding-to-integrate 可见性无效。
- 只有在一次完整 parent review 和 strict verification gate 绑定到同一个 immutable candidate 后，integrate 才能返回 target。
