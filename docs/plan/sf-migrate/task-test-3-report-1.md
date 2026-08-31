# task-test-3 测试报告

- Status: passed（strict 本机 gates）；OTP 21–29 matrix: unverified（容器运行环境权限问题）
- task: task-test-3
- generation: 3
- test profile: strict
- source branch: `codex/sf-migrate-g3-integrate`
- accepted base: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- candidate/tree: `308ca7ce86c97759fd6765f2c4c4889dc1914327` / `test: keep dynamic fixtures out of eunit discovery`
- dedicated test worktree: `/tmp/async-sf-cc-task-test-3-g3`
- test branch: `codex/sf-migrate-g3-test-3-full-tester-1`
- worktree/HEAD（测试开始及每条命令前）: clean；`git rev-parse HEAD` = `308ca7ce86c97759fd6765f2c4c4889dc1914327`；candidate 包含 accepted base。测试完成后仅新增本报告 artifact
- integrate worktree 未用于测试，未修改源码、测试、配置、workflow 或 `test.md`

## 精确命令与结果

| 命令 | 退出码 | 结果/计数 |
|---|---:|---|
| `rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl` | 0 | 6 tests passed，0 skipped |
| `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl,test/async_sf_migrate_nested_SUITE.erl` | 0 | 16 tests passed，0 skipped |
| `rebar3 ct` | 0 | 99 tests passed，0 skipped |
| `rebar3 eunit` | 0 | 33 tests，0 failures |
| `rebar3 xref` | 0 | cross reference analysis passed |
| `rebar3 dialyzer` | 0 | success typing analysis passed |
| `git diff --check` | 0 | clean |

每条命令执行前均验证 clean worktree 和上述 HEAD。工具版本：Erlang/OTP 29、ERTS 17.0.4、rebar3 3.25.1、Docker 29.6.2。

## OTP 21–29 matrix

执行命令：`rebar3 docker_ci config`（退出码 0）、`rebar3 docker_ci pull`（退出码 0）、`rebar3 docker_ci run`（退出码 1）。`erlang:21` 至 `erlang:29` 镜像均可用；9/9 目标 compile 和 xref 通过，dialyzer/eunit 按 docker_ci 默认配置跳过。

9 个目标的 Common Test 均为 93 passed、6 skipped、6 failed。失败归因一致：测试套件 `async_sf_migrate_application_SUITE:ensure_distribution/0` 启动分布式节点时，容器内 Erlang 无法创建 `/.erlang.cookie`，报 `eacces`。这是 Docker 容器用户/根目录权限设施问题，不是候选代码、夹具断言或发布路径失败；matrix 因此标为 unverified，不作为 strict 本机 gate 失败。

## 日志与产物

- Docker matrix 汇总：`/tmp/async-sf-cc-task-test-3-g3/_build/docker_ci/results/`
- 各 OTP 日志：`/tmp/async-sf-cc-task-test-3-g3/_build/docker_ci/results/{21,22,23,24,25,26,27,28,29}/ci.log`
- 各 OTP 汇总：对应目录下 `ci-summary.txt`
- Common Test/EUnit/rebar 产物：`/tmp/async-sf-cc-task-test-3-g3/_build/test/`、`/tmp/async-sf-cc-task-test-3-g3/_build/default/`
- 指定报告：`docs/plan/sf-migrate/task-test-3-report-1.md`

## 场景与跳过项

strict 本机 full CT 覆盖候选提交中的全部 99 个 Common Test 用例；聚焦应用套件 6 个、迁移及嵌套套件 16 个均通过。strict EUnit 33 个通过。无隐藏或产品性跳过。仅 OTP matrix 的每个目标因 Common Test 初始化失败而跳过其后续 eunit；该跳过由 docker_ci 的失败短路产生，已明确记录。

## 结论

候选 `308ca7ce86c97759fd6765f2c4c4889dc1914327` 在 OTP 29 专用 clean test worktree 上通过全部 strict commands。OTP 21–29 跨版本发布证据仍未验证；闭环需要修复 Docker 容器内 Erlang cookie 的可写 HOME/运行用户设施后，在同一候选提交重跑 `rebar3 docker_ci run`。
