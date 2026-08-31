# task-test-3 测试报告

- Status: passed（strict 本机 gates）；OTP 21–29 matrix: passed
- task: task-test-3
- generation: 3
- test profile: strict
- source branch: `codex/sf-migrate-g3-integrate`
- accepted base: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- candidate/tree: `b45401c1e9e0a1c1c435b3c5f463339746dccda9` / `test: support legacy otp node runners`
- verification worktree: `codex/sf-migrate-g3-integrate`
- worktree/HEAD（兼容性复验开始及每条命令前）: clean；`git rev-parse HEAD` = `b45401c1e9e0a1c1c435b3c5f463339746dccda9`；candidate 包含 accepted base
- 用户输入 `test.md`、`test_fail.md` 未修改；runner、产品代码和配置未修改

## 兼容性修正复验（b45401c）

候选 b45401c 修复 OTP 21–24 的 peer 不可用与 staged release runtime
启动兼容问题。使用包含 HOME 修复并显式启动 epmd 的本地
rebar3_docker_ci checkout 执行 rebar3 docker_ci run：

| 范围 | 结果 |
|---|---|
| OTP 21–29（9 个目标） | 全部通过 |
| 每个目标 Common Test | 6/6 |
| 每个目标 compile/xref | 通过 |
| 本地 application suite | 6/6 |
| 本地 EUnit | 33/33 |
| 本地 xref、diff check | 通过 |

OTP 21–24 的 release 节点均使用 legacy ct_slave 路径成功完成启动及
release_handler 安装场景；OTP 25–29 保持 peer 路径并通过。

## 既有 strict gates（308ca7c）

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

## OTP 21–29 matrix（首次运行）

执行命令：`rebar3 docker_ci config`（退出码 0）、`rebar3 docker_ci pull`（退出码 0）、`rebar3 docker_ci run`（退出码 1）。`erlang:21` 至 `erlang:29` 镜像均可用；9/9 目标 compile 和 xref 通过，dialyzer/eunit 按 docker_ci 默认配置跳过。

9 个目标的 Common Test 均为 93 passed、6 skipped、6 failed。失败归因一致：测试套件 `async_sf_migrate_application_SUITE:ensure_distribution/0` 启动分布式节点时，容器内 Erlang 无法创建 `/.erlang.cookie`，报 `eacces`。这是 Docker 容器用户/根目录权限设施问题，不是候选代码、夹具断言或发布路径失败；matrix 因此标为 unverified，不作为 strict 本机 gate 失败。

## OTP 21–29 matrix（目标工作树重跑）

目标工作树确认使用本地 `_checkouts/rebar3_docker_ci`（commit `6df87e1`，包含 HOME 修复）后重跑 `rebar3 docker_ci run`。HOME/cookie 错误不再出现；9 个目标仍在 Common Test 初始化阶段因 `net_kernel:start/1` 报 `Protocol 'inet_tcp': register/listen error: econnrefused`，随后 `{'EXIT', nodistribution}` 失败。直接容器复现表明，创建可写 HOME 后仍需先执行 `epmd -daemon` 才能启动分布式节点；该问题属于 Docker runner 未启动 epmd 的环境前置条件，未修改候选或产品代码。

目标工作树重跑日志：`_build/docker_ci/results/{21,22,23,24,25,26,27,28,29}/ci.log`；结果仍为 `unverified`。

## 日志与产物

- Docker matrix 汇总：`/tmp/async-sf-cc-task-test-3-g3/_build/docker_ci/results/`
- 各 OTP 日志：`/tmp/async-sf-cc-task-test-3-g3/_build/docker_ci/results/{21,22,23,24,25,26,27,28,29}/ci.log`
- 各 OTP 汇总：对应目录下 `ci-summary.txt`
- Common Test/EUnit/rebar 产物：`/tmp/async-sf-cc-task-test-3-g3/_build/test/`、`/tmp/async-sf-cc-task-test-3-g3/_build/default/`
- 指定报告：`docs/plan/sf-migrate/task-test-3-report-1.md`

## 场景与跳过项

strict 本机 full CT 覆盖候选提交中的全部 99 个 Common Test 用例；聚焦应用套件 6 个、迁移及嵌套套件 16 个均通过。strict EUnit 33 个通过。无隐藏或产品性跳过。仅 OTP matrix 的每个目标因 Common Test 初始化失败而跳过其后续 eunit；该跳过由 docker_ci 的失败短路产生，已明确记录。

## 结论

候选 b45401c1e9e0a1c1c435b3c5f463339746dccda9 已通过本地 strict gates、
review-3 PASS 以及 OTP 21–29 全矩阵。原 test_fail.md 中的 OTP 21–24
兼容性失败已闭合。
