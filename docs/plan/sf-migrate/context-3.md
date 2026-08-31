# sf-migrate 第 3 代规划上下文

- Goal: 实现 `test.md` 中的应用/发布测试设计
- Generation: `3`
- Repository: `/home/slepher/project/erl_projects/async-sf-cc`
- Common Git directory: `/home/slepher/project/erl_projects/async/.git`
- Target branch: `async-sf-cc`
- Immutable target HEAD: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- Worktree state at collection: 仅因用户所有的规范输入 `test.md` 未跟踪而处于非干净状态
- Normative input digest: `test.md` SHA-256 `f1351f61edcc16344f31811110d2f6d5d23dd33d69998ed9802778de53c22323`
- Collected: `2026-08-31`

## 当前权威要求

`test.md` 要求同一个 `sf_fixture` OTP 应用的静态 A、B、C 三个版本、完整的应用/监督者/工作进程树、外部 `test_async_controller`、三个隔离节点的启动检查，以及真实的 A->B、B->C、A->B->C 和 A->C 发布升级。待处理的父子续体和多个 MRef 必须在回复时按实际已安装的所有者版本恢复且仅恢复一次。必须覆盖完整的 T01-T18 与 N01-N10 矩阵、公开状态/进程树观察、隐私断言，以及明确的未知/重复 MRef 行为。`code:load_binary/3`、替换代码路径、休眠、不透明的迁移内部结构和推断出的多跳成功都不能作为发布证据。

用户所有的 `test.md` 仅作为输入。第 3 代不得修改、暂存或提交该文件。

## 目标上的仓库证据

- 生产代码已经暴露指定原语：`async_gen_server:call/2`、`async:promise_mref/1`，以及控制器边界上的普通 `gen_server:reply/2`。
- `async_sf_runtime` 使用当前已加载的 FrameOwner 清单解析已消费的已存储帧，并选择精确的来源声明；它不会组合迁移路径。
- `async_sf_cc` 和 `async_t` 保留一次性回调节点与直接 `parent_ref` 链接。第 2 代已经证明动态模块嵌套的订单/支付 A/B/C 回复时行为、直接 A->C 选择、混合发布、转换器/原始值路径、有界失败、同级隔离和隐私。
- 现有迁移测试套件使用 `code:load_binary/3`。它们仍是回归权威，但不能满足新的应用/发布证据要求。
- 目标上不存在 `test/app_fixtures/`、`test/test_async_controller.erl` 或 `test/async_sf_migrate_application_SUITE.erl`，仓库也没有为所需夹具配置 relx 发布。
- `rebar.config` 已提供项目测试/构建环境，并通过 `rebar3_docker_ci` 支持 OTP 21-29；无需新增依赖。

## 发布工具事实

- 本地发现 Erlang/OTP `29`、ERTS `17.0.4` 和 rebar3 `3.25.1`。
- 已安装 OTP `systools` 和 `release_handler`。已观察到的 API 包括 `systools:make_script/2`、`systools:make_relup/4`、`systools:make_tar/2`、`release_handler:unpack_release/1`、`release_handler:check_install_release/2` 和 `release_handler:install_release/2`。
- `rebar3 release` 可用，但该夹具不是仓库发布。最小且无新增依赖的测试设计是：在 Common Test 私有目录下暂存已编译的 A/B/C 夹具应用，用 OTP `systools` 生成与运行时匹配的 `.rel`/boot/relup/tar 产物，再通过公开 `release_handler` API 将其安装到专用升级节点。
- 静态 `.app` 和 `.appup` 输入仍是各版本所有的夹具文件。运行时 `.rel` 元数据必须使用实际已安装的 kernel/stdlib/sasl/async 依赖版本及不同的 A/B/C 发布版本；生成产物和节点日志只能位于 `_build`/Common Test 私有数据目录下。
- 工具存在不等于发布安装成功。只有不可变候选版本上记录的生成及 `release_handler` 命令结果、文件、节点日志和退出/状态值才是真实升级证据。

## 能力处置

| 能力 | 决策 | 原因 |
|---|---|---|
| sf-migrate 运行时/编译器行为 | reuse | 当前产品合同和第 1/2 代测试已经拥有该能力 |
| 嵌套订单/支付可观察语义 | reuse + adapt | 复用已接受的公开结果/计数器，但用静态同名应用模块表达 |
| A/B/C 应用夹具与监督树 | new test fixture | 当前不存在且被明确要求 |
| 外部延迟回复控制器 | new test helper | 必须位于 `sf_fixture_sup` 外并在应用升级中存活 |
| 隔离 Erlang 节点 | new test harness behavior | 同名应用/模块不能共存于一个代码服务器 |
| 发布组装与安装 | new test harness behavior using OTP stdlib | 无需产品发布配置或新增依赖 |
| 产品修正 | deferred finding route only | 当前没有证据授权推测性生产代码变更 |

## 规划决策

在 integrate 通道使用一个 hard `single-sup` 测试父任务。夹具、控制器、节点生命周期、发布产物和 28 个场景共享同一个状态机与失败归因边界；拆分会造成测试工具所有权重叠或额外的转发测试套件。一个高级工作者独占完整的纯测试变更，并通过一个不可变的父级审查/完整测试关卡。
