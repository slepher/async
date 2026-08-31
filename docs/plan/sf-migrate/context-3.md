# sf-migrate 第 3 代规划上下文

- Goal: 实现 `test.md` 中的 application/release 测试设计
- Generation: `3`
- Repository: `/home/slepher/project/erl_projects/async-sf-cc`
- Common Git directory: `/home/slepher/project/erl_projects/async/.git`
- Target branch: `async-sf-cc`
- Immutable target HEAD: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- Worktree state at collection: 仅因用户所有的规范输入 `test.md` 未跟踪而处于 dirty 状态
- Normative input digest: `test.md` SHA-256 `f1351f61edcc16344f31811110d2f6d5d23dd33d69998ed9802778de53c22323`
- Collected: `2026-08-31`

## 当前权威要求

`test.md` 要求同一个 `sf_fixture` OTP application 的静态 A、B、C 三个版本、完整的 application/supervisor/worker tree、外部 `test_async_controller`、三个隔离 node 的启动检查，以及真实的 A->B、B->C、A->B->C 和 A->C release upgrade。pending 的父子 continuation 和多个 MRef 必须在 reply-time 按实际已安装的 owner 版本恢复且仅恢复一次。必须覆盖完整的 T01-T18 与 N01-N10 矩阵、公开 state/tree 观察、隐私断言，以及明确的 unknown/duplicate MRef 行为。`code:load_binary/3`、替换 code path、sleep、opaque migration 内部结构和推断出的多跳成功都不能作为 release 证据。

用户所有的 `test.md` 仅作为输入。第 3 代不得修改、stage 或 commit 该文件。

## 目标上的仓库证据

- 生产代码已经暴露指定原语：`async_gen_server:call/2`、`async:promise_mref/1`，以及 controller 边界上的普通 `gen_server:reply/2`。
- `async_sf_runtime` 使用当前已加载的 FrameOwner manifest 解析已消费的 stored frame，并选择精确的 source declaration；它不会组合 migration path。
- `async_sf_cc` 和 `async_t` 保留 one-shot callback node 与直接 `parent_ref` 链接。第 2 代已经证明 dynamic-module 嵌套 order/payment A/B/C reply-time 行为、直接 A->C 选择、混合 rollout、converter/raw 路径、有界 failure、sibling 隔离和隐私。
- 现有 migration suite 使用 `code:load_binary/3`。它们仍是回归权威，但不能满足新的 application/release 证据要求。
- 目标上不存在 `test/app_fixtures/`、`test/test_async_controller.erl` 或 `test/async_sf_migrate_application_SUITE.erl`，仓库也没有为所需 fixture 配置 relx release。
- `rebar.config` 已提供项目 test/build 环境，并通过 `rebar3_docker_ci` 支持 OTP 21-29；无需新增依赖。

## Release tooling 事实

- 本地发现 Erlang/OTP `29`、ERTS `17.0.4` 和 rebar3 `3.25.1`。
- 已安装 OTP `systools` 和 `release_handler`。已观察到的 API 包括 `systools:make_script/2`、`systools:make_relup/4`、`systools:make_tar/2`、`release_handler:unpack_release/1`、`release_handler:check_install_release/2` 和 `release_handler:install_release/2`。
- `rebar3 release` 可用，但该 fixture 不是仓库 release。最小且无新增依赖的测试设计是：在 Common Test private 目录下暂存已编译的 A/B/C fixture application，用 OTP `systools` 生成与运行时匹配的 `.rel`/boot/relup/tar 产物，再通过公开 `release_handler` API 将其安装到专用 upgrade node。
- 静态 `.app` 和 `.appup` 输入仍是各版本所有的 fixture 文件。运行时 `.rel` metadata 必须使用实际已安装的 kernel/stdlib/sasl/async 依赖版本及不同的 A/B/C release 版本；生成产物和 node 日志只能位于 `_build`/Common Test private data 下。
- 工具存在不等于 release 安装成功。只有 immutable candidate 上记录的生成及 `release_handler` 命令结果、文件、node 日志和 exit/status 值才是真实 upgrade 证据。

## 能力处置

| 能力 | 决策 | 原因 |
|---|---|---|
| sf-migrate runtime/compiler 行为 | reuse | 当前产品合同和第 1/2 代测试已经拥有该能力 |
| 嵌套 order/payment 可观察语义 | reuse + adapt | 复用已接受的公开 outcome/counter，但用静态同名 application module 表达 |
| A/B/C application fixture 与 supervisor tree | new test fixture | 当前不存在且被明确要求 |
| 外部 held-reply controller | new test helper | 必须位于 `sf_fixture_sup` 外并在 application upgrade 中存活 |
| 隔离 Erlang node | new test harness behavior | 同名 application/module 不能共存于一个 code server |
| release 组装与安装 | new test harness behavior using OTP stdlib | 无需产品 release 配置或新增依赖 |
| 产品修正 | deferred finding route only | 当前没有证据授权推测性生产代码变更 |

## 规划决策

在 integrate lane 使用一个 hard `single-sup` test parent。fixture、controller、node lifecycle、release 产物和 28 个场景共享同一个 state machine 与 failure 归因边界；拆分会造成 harness 所有权重叠或额外的转发 suite。一个 superior worker 独占完整的纯测试变更，并通过一个 immutable parent review/full-test gate。
