# sf-migrate 第 3 代应用发布规划

- Plan status: accepted
- Goal: `sf-migrate`
- Generation: `3`
- Context: context-3.md
- Evidence target: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- Target branch: `async-sf-cc`
- Normative input: 用户所有的 `test.md`，SHA-256 为 `f1351f61edcc16344f31811110d2f6d5d23dd33d69998ed9802778de53c22323`
- Profile: strict

## 实质成果

增加公开黑盒 Common Test 证据，证明同一个 `sf_fixture` OTP 应用的静态 A、B、C 版本可在独立节点上以完整监督树启动，并可在专用节点上通过生成的 OTP 发布脚本和 `release_handler` 完成升级。全部 T01-T18 和 N01-N10 场景必须证明延迟 MRef 身份、任意回复顺序、回复时精确版本选择、直接 A->C 行为、父子续体保留、一次性完成、进程树/控制器稳定性和有界的私有失败。

## 范围

范围内包括：`test/app_fixtures/` 下的版本化夹具源代码/应用/appup 输入、一个外部控制器辅助模块、一个应用发布 Common Test 测试套件、测试私有 `_build` 路径下运行时生成的发布产物、聚焦回归、严格的不可变验证，以及测试发现任何产品缺陷时的修正路由。

排除项包括：修改 `src/`、`include/`、README/OpenSpec、仓库发布配置、依赖、现有测试/夹具、第 1/2 代产物和 `test.md`；提前迁移；多跳/版本图逻辑；私有回调/计划/桥接断言；基于休眠的同步；以及测试工作者对产品代码的任何修复。

## 验收标准

1. A/B/C 源代码目录使用相同的应用与模块名称、不同且可观察的版本标记、有效的 `.app` 和 `.appup` 元数据，以及 `sf_fixture_sup -> order_sup/order_worker + payment_sup/payment_worker` 进程树。
2. `test_async_controller` 在夹具进程树外运行，保留精确的 `{Pid,MRef}` 调用者，支持多个待处理调用和任意顺序回复，并能区分未知与已回复引用，且不会错误改变状态。
3. 三个隔离节点各自只启动一个夹具版本，并通过公开 API/进程树查询证明应用、监督者、工作进程和控制器相互独立。
4. 专用升级节点使用生成的 `.rel`/boot/relup/tar 产物和公开 `release_handler` 安装执行 A->B->C 与 A->C；绝不使用 `code:load_binary/3` 或替换代码路径作为升级证据。
5. T01-T18 和 N01-N10 每一行都有明确的测试用例/矩阵映射和 `test.md` 要求的断言，包括直接 A->C/无 B 计数器、混合 A/B/C 未完成帧、嵌套直接父级路由、错误/隐私行为，以及无重复完成。
6. 聚焦迁移回归与完整仓库关卡在同一个已审查的不可变候选版本上通过。因环境不可用的发布或矩阵证据必须附精确证据并报告为 `unverified`，绝不能称为 `passed`。

## 测试能力处置

复用当前公开 async API、迁移结果/计数器、Common Test、OTP 应用/监督者 API，以及 OTP `systools`/`release_handler`。不扩展生产接口。只增加静态夹具、外部控制器，以及取得指定证据所需的最小测试套件内发布/节点工具。不增加 relx 配置、依赖、通用发布抽象，也不复制第 2 代动态迁移辅助模块。

## 执行拓扑

一个 `single-sup` 父任务在 integrate 通道中拥有完整测试生命周期。连续的跨节点/发布/应用状态与共享静态模块使隔离的默认工作者移交不安全。当前不规划编码；若发现产品问题，之后再打开有界修复合同，并且必须先走完 `coding -> target -> integrate -> target` 才能重跑。

## task-test-3

- Outcome: 实现并验证 `task-test-3.md` 中完整的应用/发布设计。
- Lane: integrate
- Depends on: none
- Covers: none
- Base: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- Worker role: `sup_coding_worker`
- Owned paths: `test/app_fixtures/**`, `test/test_async_controller.erl`, `test/async_sf_migrate_application_SUITE.erl`
- Completion: 精确候选版本满足合同、父级审查通过、严格验证记录全部必需命令与发布证据，且 dispatcher 将 integrate 合并到 target。

## 验证与证据边界

聚焦应用测试套件、现有迁移测试套件、完整 CT/EUnit/xref/dialyzer 关卡和 `git diff --check` 均在同一个不可变候选版本上运行。发布测试用例还要保留 OTP/rebar 版本、公开工具调用/结果、`.app`/`.appup`/`.rel`/boot/relup/tar 路径、`release_handler:which_releases/0` 转换、节点日志，以及进程/进程树/控制器观察。

若发布生成/安装或可选 OTP 容器矩阵因环境先决条件不可用，必须记录精确命令/调用、工具/运行时版本、状态/退出码、输出和产物边界。独立节点测试仍须运行。跳过/不可用的发布测试用例是 `unverified`，而不是 `passed`；产品/测试断言失败是失败，不是先决条件问题。dispatcher 负责最终披露和在匹配环境中的重跑闭环。

## 合并与修正关卡

测试父任务的已接受路由是 `integrate -> target`。发现产品问题时，测试工作以最小复现停止；dispatcher 在 coding 中创建 `task-test-3-fix-N`，将已审查的修复按 `coding -> target` 合并，从该精确 target 刷新 integrate，并在 `integrate -> target` 前重跑和审查已失效的测试。禁止直接从 coding 合并到 integrate。

## 可观察完成条件

target 只包含已接受且由测试所有的新增内容；所有可用阻塞证据均通过；每个不可用项都明确标为 `unverified`，并有所有者与闭环；不使用私有或伪造的升级机制；除非用户另行决定提交，`test.md` 保持不变且未跟踪。
