# sf-migrate 第 3 代通道清单

- Lanes status: accepted
- Plan: plan-3.md
- Concurrency: lanes
- Merge policy: merge-ready FIFO
- Merge concurrency: 1

## 控制工作树

- Name: async-sf-cc
- Path: /home/slepher/project/erl_projects/async-sf-cc
- Target branch: async-sf-cc
- Target base: 7552a0c7d6bd1cd167b9822534ad7327352e1f81

## 编码通道

- Worktree name: coding
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-migrate-g3-coding
- Serial: true

## 集成通道

- Worktree name: integrate
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-migrate-g3-integrate
- Serial: true

## 默认值

- Default coding lane: coding
- Default integration lane: integrate
- Route: coding -> target -> integrate -> target
- Project bootstrap: 遵循仓库说明和现有 rebar 设置；通用通道清单不引入项目专用引导命令
- Product correction route: task-test-3 发现 -> coding 中的 task-test-3-fix-N -> target -> integrate 刷新/重跑 -> target

## 阶段 task-test-3

- Kind: test
- Mode: hard
- Mode basis: 一个共享的发布/应用/节点/控制器生命周期覆盖全部夹具版本和场景；需要连续的跨版本与分布式节点推理
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Parent verification role: full_tester
- Lane: integrate
- Depends on: none
- Covers: none
- Base: 7552a0c7d6bd1cd167b9822534ad7327352e1f81

## 所有权与可见性关卡

- `task-test-3` 独占 `test/app_fixtures/**`、`test/test_async_controller.erl` 和 `test/async_sf_migrate_application_SUITE.erl`。
- 对工作者而言，生产代码、现有测试、文档/规范、配置、工作流产物和 `test.md` 均为只读。
- 分派前必须将 integrate 干净地重新绑定到精确 target base。现有第 1/2 代通道绑定属于历史记录，不得复用为执行身份。
- 产品修正只有在已审查的 coding 候选版本到达 target，且 integrate 从该精确 target HEAD 刷新后才可见。通道本地或直接从 coding 到 integrate 的可见性无效。
- 只有在一次完整父级审查和严格验证关卡绑定到同一个不可变候选版本后，integrate 才能返回 target。
