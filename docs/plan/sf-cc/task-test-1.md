# Test Task Contract

- Task: task-test-1
- Bundle generation: 1
- Phase artifact: task-test-1.md
- Plan: plan-1.md
- Lane manifest: lanes-1.md
- Context: context-1.md
- Evidence target: task-coding-1 accepted immutable candidate
- Task kind: test
- Mode: hard
- Mode basis: macro/compiler、runtime lifecycle、old-code compatibility、frame integration、matcher transaction 与三 callbacks backends 必须作为同一跨版本验收矩阵运行
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Lane: integrate
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-cc-g1-integrate
- Base: task-coding-1 accepted merge on control target
- Depends on: none
- Covers: task-coding-1

## Bounded Goal

针对 `task-coding-1` 的不可变 candidate，独立建立并运行 document-driven 黑盒/集成测试，证明 sf-cc R1-R19 与 sf-do changelog 的 ownership、terminal、legacy、Module/Vsn、resolution 和普通行为兼容合同。

## Exact normative objectives

- 新 sf-prefixed macro 的 external/local/anonymous/variable/expression/arity acceptance matrix 与 source-position diagnostics。
- every-runner adoption、descriptor/Context persistence、三个 callback mode result contract。
- 三元/四元 callback decode、AccRef/CcRef independent scope、CcRef store classification、cleanup-before-invoke 与 exactly-once。
- message-before-reply、reply/timeout/DOWN、protected structural failure、unprotected purge legacy outcome。
- frame terminal-only CcRef、nested multi-module summary、literal Vsn、matcher privacy/atomicity、active-fail dedupe/per-CcRef continuation。
- map、orddict/list、dict backends，以及 ordinary wait/runner/sf-do observable regression。

## Existing coverage not to duplicate

- E-18 的 10-case `async_sf_SUITE` 已覆盖旧 source/frame/lifecycle/capture/identity 基线；只更新旧 terminal shape 并保留这些行为，不把其通过当作新 CcRef 证明。
- E-19 的 `async_t_SUITE` 已覆盖 ordinary timeout/message/parallel/local AccRef；新用例只增加 CcRef propagation、cleanup 与兼容断言。
- E-22 的 historical suites 仅证明普通 v1-v5 reply；本任务必须新增真实 old three-tuple State fixture，而不是把这些 suites 重命名为 upgrade coverage。

## Owned paths

- `test/async_sf_SUITE.erl`
- `test/async_t_SUITE.erl`
- `test/async_histories_SUITE.erl`
- `test/async_m_v5.erl` 与 `test/async_v5.erl`（仅当 real old-state fixture 复用该历史边界）
- `test/async_sf_cc_SUITE.erl`（new，优先作为主 document-driven integration suite）
- `test/async_sf_cc_macro_SUITE.erl`（new，仅在 compiler fixture 不能清晰并入主 suite 时）
- `test/async_sf_cc_fixture.erl`（new）
- `test/async_sf_cc_old_callback_fixture.erl`（new，如需要）

不得修改 `src/`、`include/`、`rebar.config`、OpenSpec、workflow artifacts 或 coding candidate。产品失败必须作为 test finding 返回，由 dispatcher 创建 fix contract。

## Ordered work

1. 冻结 candidate revision，先运行现有 sf-do 与 ordinary baseline，记录旧 terminal assertion 的唯一预期迁移。
2. 建 compiler fixture：三个 runner 的 accepted external arity 与 local/anonymous/variable/expression/wrong-arity、多诊断 source position。
3. 建 runner/descriptor matrix：immediate+suspended legacy/frame Promise，async `/2`、direct `/2`、direct `/3`、invalid return、callback crash 与 opaque Context。
4. 建 lifecycle matrix：sequential suspension、parallel/shared CcRef、AccRef rebinding、cleanup-before-callback、unrelated resources preserved、late reply、duplicate completion/fail。
5. 建 routing/upgrade matrix：message-before-reply、timeout、DOWN、map/list/dict、standalone CcRef unhandled、old three-tuple resumable/unprotected、purged old tuple tagged outcome、business exception/nested `undef` 不误判。
6. 建 frame matrix：CcRef-only terminal、field/terminal consistency、damaged continuation direct fallback、nested owner modules、literal/undefined Vsn 与 reply-time per-node resolution。
7. 建 inspect/fail matrix：sanitized privacy、one matcher call/order、exception/non-boolean bit-for-bit atomicity、CcRef dedupe、unprotected retained、cc_failed 后继续与 sorted commit。
8. 运行 focused suite 后执行完整验证矩阵，生成精确 test report 内容与 coverage map。

## Focused self-run

- `rebar3 ct --suite async_sf_cc_SUITE`
- 若独立创建 macro suite：`rebar3 ct --suite async_sf_cc_macro_SUITE`
- `rebar3 ct --suite async_sf_SUITE`
- `rebar3 ct --suite async_t_SUITE`

## Full validation

- `rebar3 compile`
- `rebar3 eunit`
- `rebar3 ct`
- `rebar3 xref`
- `rebar3 dialyzer`
- repository-supported OTP compatibility matrix，必须包含真实 old three-tuple State fixture；若环境不能运行，明确列为 unavailable，不能宣称通过。
- `npx openspec validate sf-cc --strict`（若仓库安装的 OpenSpec CLI 使用不同等价 argv，停止并由 dispatcher确认，不自行改变 normative command）。

## Required evidence

- candidate revision、每条命令/退出码/环境与测试报告原始位置。
- R1-R19 与 sf-do changelog 条目到具体 test case 的 coverage matrix。
- 至少一次 callback invocation counter 与 CleanState observation，证明 cleanup-before-invoke/exactly-once。
- old 三元 fixture 的构造来源、protected/unprotected 区分及 narrow unavailable classifier 的正反例。
- matcher summary 的允许 keys 与 Env/State/timer/monitor/accumulator/final callback/Context no-leak 断言。
- 未运行、flaky、平台限制或无法证明的项目必须显式列出。

## Observable completion

- focused 与 full validation 在同一不可变 candidate 上满足门槛；无失败、无未解释 warning/regression。
- 每项 R1-R19 至少有直接可观察测试，sf-do ownership/resolution 条目也有明确 coverage。
- 旧 ordinary/sf-do 行为保留；新 terminal/record/store shape 只在合同要求处改变。
- test report 足以让 reviewer 在不读取 moving worktree 的情况下判断通过或 changes_required。

## Stop conditions

- candidate 与 coding handoff revision 不一致；
- fixture 需要修改非 owned path 或 production code；
- 文档不能决定期望结果、OpenSpec CLI 语义不明或 supported OTP matrix 不可识别；
- baseline 不可复现、测试发现产品 defect、隐私泄露、重复 callback 或 State mutation atomicity 失败。

## Handoff

返回 `Status`、不可变 candidate、测试变更、完整命令结果、R1-R19 coverage、findings、未运行项与可原样持久化为 `task-test-1-report.md` 的报告正文。不得修产品代码、stage/commit、维护 checklist 或自行创建 fix task。
