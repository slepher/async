# 测试任务合同

- Task: task-test-3
- Bundle generation: 3
- Phase artifact: task-test-3.md
- Plan: plan-3.md
- Lane manifest: lanes-3.md
- Context: context-3.md
- Evidence target: 7552a0c7d6bd1cd167b9822534ad7327352e1f81
- Task kind: test
- Mode: hard
- Mode basis: 夹具编译、发布生成/安装、节点生命周期、控制器身份和全部场景断言构成一个连续的跨版本状态机
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Parent verification role: full_tester
- Test profile: strict
- Lane: integrate
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-migrate-g3-integrate
- Base: 7552a0c7d6bd1cd167b9822534ad7327352e1f81
- Depends on: none
- Covers: none

## 有界目标

将 `test.md` 实现为真实、公开的黑盒应用与发布证据。构建静态同名的 A/B/C `sf_fixture` 应用，将延迟回复控制器保留在其进程树之外，执行隔离节点与真实发布升级场景，并在不改变产品行为或现有证据的前提下验证每个必需的 T/N 矩阵行。

## 任务所有路径

- `test/app_fixtures/**`
- `test/test_async_controller.erl`
- `test/async_sf_migrate_application_SUITE.erl`

所有 `src/`、`include/`、现有测试文件/夹具、README/OpenSpec、`rebar.config*`、工作流、`docs/plan/**` 和 `test.md` 均为只读。生成的发布文件必须位于 Common Test 私有数据目录或 `_build/test` 下；绝不能将生成产物写入任务拥有的源代码目录。

## 必需的夹具与控制器工作

1. 创建 A/B/C 目录，使用相同的 `sf_fixture` 应用名称及相同的模块名称 `sf_fixture_app`、`sf_fixture_sup`、`order_flow` 和 `payment_flow`。每个版本具有不同的 `.app` `vsn`/可观察标记及版本正确的 `.appup`；不要重复本可统一拥有所需监督者和工作进程回调子句的模块。
2. 暴露完整的注册进程树 `sf_fixture_sup -> order_sup -> order_worker` 和 `sf_fixture_sup -> payment_sup -> payment_worker`。公开夹具 API/消息报告应用版本、阶段、工作进程所有者、完成次数、计数器和有界错误；测试不检查记录、生成的桥接名称、计划、AST 数据或回调内部结构。
3. 将 `test_async_controller` 实现为由测试套件启动的独立 `gen_server`。存储从 `async_gen_server:call/2` 收到的精确 `{Pid,MRef}` 值，发出 `{controller_held,MRef,Request}`，仅通过 `gen_server:reply/2` 回复，仅删除一次活动条目，保留足够的已回复身份以区分 `{error,already_replied}` 与 `{error,unknown_mref}`，并仅暴露规范要求的待处理项/计数/身份观察。
4. 夹具工作进程仅使用 `async_gen_server:call(Controller,Request)` 和 `async:promise_mref(MRef)`。订单和支付创建可独立识别的父/子 MRef、摘要和完成标记；子结果仅重新进入其存储的直接父级。

## 必需的节点与发布工具

1. 启动三个代码路径隔离的临时分布式节点，每个节点使用一个夹具版本及独立控制器。验证应用列表、应用/监督者/工作进程 pid、注册进程树结构、版本标记以及跨节点/控制器的独立性。使用延迟事件、RPC 结果、监视器和显式回复进行同步；不得休眠。
2. 在测试套件私有发布根目录下暂存已编译的 A/B/C 夹具应用及当前不可变的 async/依赖产物。使用公开 OTP `systools` 生成运行时正确的 A/B/C `.rel`、script/boot、relup 和发布 tar 产物；保持一个发布名称和不同的发布版本。
3. 启动独立的 A 发布节点，然后通过 `release_handler:unpack_release`、`check_install_release` 和 `install_release`（或已安装 OTP 所要求的精确公开等价项）执行 A->B->C 和全新的 A->C。断言 `which_releases` 以及应用/进程树/版本转换。代码路径更改和 `code:load_binary/3` 既不能准备升级，也不能准备升级证明。
4. 在每次安装过程中保留控制器 pid、控制器状态、未完成 MRef、应用可用性和进程树可查询性。清除断言只能遵循生成的发布指令和 OTP 工具所允许的行为。
5. 为审查/完整测试证据捕获 OTP/ERTS/rebar 版本、每个生成/安装调用及结果、夹具元数据与生成产物路径、节点命令/日志/状态、发布转换和场景观察。

## 场景覆盖

- 实现 `test.md` 的每一行 T01-T18 和 N01-N10。让标识符在 Common Test 测试用例名称、组或一一对应的场景表中可见，使遗漏和失败可归因。
- T01-T03 是 A/B/C 的无升级基线。T04-T07 区分 A->B、B->C、A->B->C 后由 C 直接处理和全新的 A->C；直接测试用例证明 B 桥接/转换器/主体计数器不会运行。
- T08-T10 证明升级前、升级间和升级后的回复。T11-T18 证明多个来源版本、同一来源的多个 MRef、任意顺序、重复/未知回复语义、控制器/进程树连续性、并发的新旧调用，以及每个帧所存版本的所有权。
- N01-N10 证明独立的父/子所有权、混合订单/支付发布、直接 A->C、原始值/转换器组合、乱序父/子回复、有界子级失败、同级隔离、无 Env/内部信息泄漏、迟到/重复子级处理，以及严格的一次外层完成。
- 断言 `test.md` 所列的每个启动前/启动/延迟/升级/回复/失败检查点。原始请求计数为一；任何历史主体/转换器都不会提前运行；只有回复时选中的路由/当前步骤递增；控制器待处理状态及回调/完成状态恰好关闭一次。

## 复用与简洁性约束

- 复用现有公开 async API、Common Test/EUnit 断言宏、OTP 应用/监督者/RPC/发布 API，以及已接受的第 2 代结果/计数器词汇。测试任务审查判断可观察正确性、所有权和证据；它不对测试代码应用 implementation-simplicity 审计。
- 直接使用 OTP `systools` 和 `release_handler`；不增加依赖、仓库发布配置、通用框架、私有发布状态读取器、轮询休眠或仅供生产代码使用的测试接缝。
- 将发布/节点辅助函数保留在这一个测试套件中，除非必须存在单独编译的控制器或强制要求的静态夹具模块。生成依赖运行时的 `.rel` 文件，不签入 OTP 版本专用副本。
- 不削弱或重写现有测试。不将产品失败归类为跳过，不改变预期行为以迎合实现，也不修复产品代码。

## 聚焦自运行

- `rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`
- `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl,test/async_sf_migrate_nested_SUITE.erl`
- `git diff --check`

工作者必须运行其新增的测试套件，报告精确命令/退出码/测试用例计数/跳过项/日志路径，并仅修正所有的测试夹具/工具/断言缺陷。遇到产品失败时，停止并提供最小公开复现和精确发布/节点证据。

## 阻塞性自验证

- Commands: `rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`; `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl,test/async_sf_migrate_nested_SUITE.erl`; `rebar3 ct`; `rebar3 eunit`; `rebar3 xref`; `rebar3 dialyzer`; `git diff --check`
- Binding: 在完成精确分支/HEAD/target 祖先关系/干净度/变更路径检查后，所有命令都在已审查的不可变累积候选版本上运行
- Failure rule: 任何可用命令失败、断言失败、隐藏跳过、伪造升级路径、非所有变更、私有数据依赖、值泄漏或无法解释的场景遗漏都会阻塞合并就绪

## 环境依赖验证

- Commands: 应用测试套件内的真实发布测试用例；当所需容器运行时/镜像可用时，运行可选的 `rebar3 docker_ci` OTP 21-29 矩阵
- Owner: `full_tester` 生成证据；dispatcher 保留披露和重跑闭环责任
- Unavailable classification: 仅限缺失/不兼容的外部运行时、发布先决条件、容器服务/镜像或操作系统设施，并提供精确命令/调用、OTP/rebar/运行时版本、状态/退出码、输出、节点日志和最后生成的产物
- Not unavailable: 编译错误、错误的夹具元数据/appup/relup、候选版本导致的发布安装拒绝、断言失败、场景启动后超时、产品失败、没有精确先决条件证据的跳过测试用例，或使用替代路径
- Closure gate: 在匹配且已授权的环境中，对同一个不可变候选版本重跑精确发布测试用例或矩阵命令；如果候选版本变更，则重复父级审查和所有受影响关卡
- Reporting: 不可用的发布证据为 `unverified`，绝不是 `passed`；独立节点和非发布测试用例仍单独执行和报告

## 必需证据

- 精确的 target base、integrate 起点、已审查候选版本/树、变更路径、工具/运行时版本、命令、退出码、测试用例计数、跳过项和 CT 日志/产物路径。
- A/B/C 应用/模块/版本表；`.app`/`.appup` 输入；生成的 `.rel`/boot/relup/tar 路径和哈希；公开 `release_handler` 转换/结果；节点命令/日志/状态。
- 每次安装前后的公开进程树/控制器 pid 和待处理计数快照；T01-T18 和 N01-N10 覆盖/结果矩阵；直接选择、无重放/串链、任意顺序、一次性完成和嵌套直接父级路由的计数器/结果证据。
- 隐私/禁用路径审计，证明没有 Env/Outcome/State/回调记录/fun/敏感哨兵值泄漏，没有生成名称/计划断言，没有休眠排序，也没有将 `code:load_binary/3` 或代码路径替换用作发布证据。
- 每个未验证发布或矩阵项的精确不可用证据记录及闭环所有者。

## 可观察完成条件

所有任务拥有的文件均实现指定的夹具、控制器、拓扑、发布工具和场景矩阵；聚焦及严格的可用关卡在同一个不可变候选版本上通过；发布证据真实且已记录，或明确标为未验证；不存在产品/非任务所有的变更或未解决的测试缺陷；且父级审查建议合并就绪。

## 合并与修正关卡

- 测试变更仅在父级审查和严格验证绑定到同一候选版本后按 `integrate -> target` 合并。
- 产品发现将精确复现/证据返回给 dispatcher。任何修复都在 coding 中使用 `task-test-3-fix-N`，并仅通过 `coding -> target -> integrate -> target` 到达测试。
- 不允许直接从 coding 合并到 integrate、在测试侧修复产品、接受断言失败、隐藏跳过、过期证据或替代升级证明。

## 父级移交

返回 `Status`、`Result`、精确 base/candidate/tree、变更路径、场景矩阵、发布/应用/控制器证据、命令/退出码/日志、带闭环的不可用项、发现和合并就绪建议。不要暂存、提交、合并、更新状态、配置工作树、编辑排除路径或宣布计划事项完成。
