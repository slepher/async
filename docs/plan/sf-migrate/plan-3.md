# sf-migrate 第 3 代 application release 规划

- Plan status: accepted
- Goal: `sf-migrate`
- Generation: `3`
- Context: context-3.md
- Evidence target: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- Target branch: `async-sf-cc`
- Normative input: 用户所有的 `test.md`，SHA-256 为 `f1351f61edcc16344f31811110d2f6d5d23dd33d69998ed9802778de53c22323`
- Profile: strict

## Material outcome

增加公开黑盒 Common Test 证据，证明同一个 `sf_fixture` OTP application 的静态 A、B、C 版本可在独立 node 上以完整 supervisor tree 启动，并可在专用 node 上通过生成的 OTP release script 和 `release_handler` 完成 upgrade。全部 T01-T18 和 N01-N10 场景必须证明 held MRef identity、任意 reply 顺序、reply-time 精确版本选择、直接 A->C 行为、父子 continuation 保留、one-shot 完成、tree/controller 稳定性和有界的私有 failure。

## Scope

范围内包括：`test/app_fixtures/` 下的版本化 fixture source/application/appup 输入、一个外部 controller helper、一个 application-release Common Test suite、test-private `_build` 路径下运行时生成的 release 产物、聚焦回归、strict immutable 验证，以及测试发现任何产品缺陷时的修正路由。

排除项包括：修改 `src/`、`include/`、README/OpenSpec、仓库 release 配置、依赖、现有 test/fixture、第 1/2 代产物和 `test.md`；eager migration；multi-hop/version-graph 逻辑；私有 callback/plan/bridge 断言；基于 sleep 的同步；以及 test worker 对产品代码的任何修复。

## Acceptance

1. A/B/C source 目录使用相同的 application 与 module 名称、不同且可观察的版本 marker、有效的 `.app` 和 `.appup` metadata，以及 `sf_fixture_sup -> order_sup/order_worker + payment_sup/payment_worker` tree。
2. `test_async_controller` 在 fixture tree 外运行，保留精确的 `{Pid,MRef}` caller，支持多个 pending call 和任意顺序 reply，并能区分 unknown 与 already-replied reference，且不会错误改变 state。
3. 三个隔离 node 各自只启动一个 fixture 版本，并通过公开 API/tree 查询证明 application、supervisor、worker 和 controller 相互独立。
4. 专用 upgrade node 使用生成的 `.rel`/boot/relup/tar 产物和公开 `release_handler` 安装执行 A->B->C 与 A->C；绝不使用 `code:load_binary/3` 或替换 code path 作为 upgrade 证据。
5. T01-T18 和 N01-N10 每一行都有明确的 case/matrix 映射和 `test.md` 要求的断言，包括直接 A->C/no-B counter、混合 A/B/C outstanding frame、嵌套 direct-parent routing、错误/隐私行为，以及无重复完成。
6. 聚焦 migration 回归与完整仓库 gate 在同一个已 review 的 immutable candidate 上通过。因环境不可用的 release 或 matrix 证据必须附精确证据并报告为 unverified，绝不能称为 passed。

## Test capability disposition

复用当前公开 async API、migration outcome/counter、Common Test、OTP application/supervisor API，以及 OTP `systools`/`release_handler`。不扩展生产接口。只增加静态 fixture、外部 controller，以及取得指定证据所需的最小 suite-local release/node harness。不增加 relx 配置、依赖、通用 release 抽象，也不复制第 2 代 dynamic migration helper。

## Execution topology

一个 `single-sup` parent 在 integrate lane 中拥有完整 test lifecycle。连续的跨 node/release/application state 与共享静态 module 使隔离的 default-worker handoff 不安全。当前不规划 coding；若发现产品问题，之后再打开有界 fix contract，并且必须先走完 `coding -> target -> integrate -> target` 才能重跑。

## task-test-3

- Outcome: 实现并验证 `task-test-3.md` 中完整的 application/release 设计。
- Lane: integrate
- Depends on: none
- Covers: none
- Base: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- Worker role: `sup_coding_worker`
- Owned paths: `test/app_fixtures/**`, `test/test_async_controller.erl`, `test/async_sf_migrate_application_SUITE.erl`
- Completion: 精确 candidate 满足合同、parent review 通过、strict 验证记录全部必需命令与 release 证据，且 dispatcher 将 integrate merge 到 target。

## Verification and evidence boundary

聚焦 application suite、现有 migration suite、完整 CT/EUnit/xref/dialyzer gate 和 `git diff --check` 均在同一个 immutable candidate 上运行。release case 还要保留 OTP/rebar 版本、公开 tooling call/result、`.app`/`.appup`/`.rel`/boot/relup/tar 路径、`release_handler:which_releases/0` transition、node 日志，以及 process/tree/controller 观察。

若 release 生成/安装或可选 OTP container matrix 因环境 prerequisite 不可用，必须记录精确 command/call、tool/runtime 版本、status/exit、输出和 artifact boundary。独立 node 测试仍须运行。skipped/unavailable release case 是 unverified，而不是 passed；产品/测试 assertion failure 是 failure，不是 prerequisite。dispatcher 负责最终披露和在匹配环境中的重跑闭环。

## Merge and correction gate

test parent 的已接受路由是 `integrate -> target`。发现产品问题时，测试工作以最小复现停止；dispatcher 在 coding 中创建 `task-test-3-fix-N`，将已 review 的修复按 `coding -> target` merge，从该精确 target 刷新 integrate，并在 `integrate -> target` 前重跑和 review 已失效的测试。禁止直接 coding-to-integrate merge。

## Observable completion

target 只包含已接受且测试所有的新增内容；所有可用 blocking evidence 均为 green；每个不可用项都明确标为 unverified，并有 owner 与 closure；不使用私有或伪造的 upgrade 机制；除非用户另行决定 commit，`test.md` 保持不变且未跟踪。
