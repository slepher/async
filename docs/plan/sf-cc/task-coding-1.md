# Coding Task Contract

- Task: task-coding-1
- Bundle generation: 1
- Phase artifact: task-coding-1.md
- Plan: plan-1.md
- Lane manifest: lanes-1.md
- Context: context-1.md
- Evidence target: 9691772e7e260e32cb5c7fd11c0cd51563b3530a
- Task kind: coding
- Mode: hard
- Mode basis: 跨 `async_t`、`async_r_t`、`async_util`、`async_m`、`async_sf_runtime` 与新增 owner 的 lifecycle/compatibility shared invariant 必须由一个 superior worker 保持
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Lane: coding
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-cc-g1-coding
- Base: 9691772e7e260e32cb5c7fd11c0cd51563b3530a
- Depends on: none
- Covers: none

## Bounded Goal

实现 `plan-1.md` 定义的完整 sf-cc capability，使所有 post-upgrade public runners 共享一次性 CcRef final owner，并把 sf-do frame terminal/structural failure 接入同一 completion primitive，同时保留 ordinary API 与旧三元 tuple 的兼容合同。

## Exact normative excerpts

- `sf-cc/spec.md` R2：每次 post-upgrade public runner invocation 都创建 CcRef；只有升级前已 pending 的旧三元 tuple 没有 CcRef。
- R9/R10：用户 final callback 前必须完成 preflight、take 与 owned lifecycle cleanup；take 后的任何失败不得恢复、重试或二次调用。
- R14/R15：三元 tuple decode 为 `sf_cc_mref = undefined`；仅当 old-code purge 被窄分类确认时返回原 action input 的 `{legacy_outcome, Outcome, CleanState}`。
- R13：frame terminal 仅为 `#{kind => sf_cc_ref, protocol => 1, mref => CcRef}`，且与 callback field 一致。
- `sf-do/changelog.md`：`sf-do` 不再拥有完整 descriptor/runner/final invoker；frame/manifest 保存 owner Module 与 literal Vsn，并在 reply-time 对每个 owner module 独立 resolve。

## Context evidence

- E-10/E-12/E-13：callback record/readers 分散，`callback_exists/1` 当前只处理 map/two-field record。
- E-11/E-14：ordinary runner 与 reader-local 目前只传播 raw AccRef，并通过旧 wrapper 做 final cleanup。
- E-15/E-18：sf runtime 当前内嵌完整 descriptor、递归寻找 terminal 且先 invoke 后 cleanup；测试固化旧 shape。
- E-16/E-17/E-20：frame producer 无 Module/Vsn，公共 include 无 macro provider，所有新 symbols 均未实现。

## Owned paths

- `include/async.hrl`
- `include/async_internal.hrl`（new）
- `src/async_sf_cc_macro.erl`（new）
- `src/async_sf_cc.erl`（new）
- `src/async_m.erl`
- `src/async_t.erl`
- `src/async_r_t.erl`
- `src/async_util.erl`
- `src/async_sf_runtime.erl`
- `src/async_sf_transform.erl`
- `src/monad_frame.erl`（仅在现有 frame metadata encoding 确实需要时）
- `rebar.config`（仅在注册独立 macro provider/build wiring 确实需要时）

不得修改 `test/`、其他 OpenSpec change、`openspec/changes/*/tasks.md`、workflow artifacts 或 sf-migrate 实现。若正确实现需要 owned paths 之外的产品路径，立即停止并报告精确路径与理由。

## Constraints and invariants

- 一个 capability 只有一个 owner：`async_sf_cc` 拥有 final descriptor/CcRef/completion/inspection/fail；`async_sf_runtime` 只执行 frame 并委托 completion。
- shared header 替换重复 record 声明；所有 reader 必须通过一个 decoder/classifier，不保留旁路 pattern。
- `cc` 与 `acc_ref` 原语义不变；CcRef 不藏入 AccRef，不使用 sidecar ActionMRef mapping。
- macro 保留原 external-fun AST，不构造 MFA tuple，不在 runtime 用 `fun_info` 追溯 source origin。
- cleanup 只删除同 CcRef owned entries/resources，保留 unrelated callbacks；accumulator 仅在无 surviving reference 时删除。
- message 不消费 action/CcRef；ordinary business exception 与 nested `undef` 不得误归类为 unavailable old code。
- `/3` 使用现有 State tag/arity compatibility；callback 调用前 State 已 clean；async completion effect 不隐式复用 consumed CcRef。
- owner-resolution 只建立当前 frame 的逐节点边界；不得实现 sf-migrate 的 historical block、migrator/catalog/composition/downgrade policy。

## Ordered work

1. 建立 shared callback header、decoder 与 value classifier；迁移所有当前 reader/writer/kind walker，验证 map/list/dict backend 与 old tuple decode。
2. 将 `async_r_t` reader-local 规范化为兼容 raw Ref 的 internal scope；`get_local_ref` 行为不变，新增 internal CcRef reader，`local_ref/3` 只改 AccRef。
3. 新建 `async_sf_cc` descriptor/entry constructors 和 validators，完成 preflight → take → owned cleanup → invoke 状态机及 mode/legacy normalization。
4. 将 promise suspension、reply/message/timeout/DOWN executor 接入 scope 与 CcRef；先保证 routing/classification 与 late/duplicate unhandled，再接 public runner。
5. 新建独立 Astranaut provider并接入 include；实现三个 sf-prefixed runtime targets，保持 external-fun AST 与 compiler diagnostic position。
6. 把 ordinary runners 与 wait 委托 common core，以现有 adapter/library-owned completion 保留 observable result；删除被替代的旧 final cleanup owner。
7. 将 frame terminal 替换为 CcRef reference，令 normal/structural/directional failure 使用 decoded callback field 与 `async_sf_cc:complete`；移除完整 descriptor invoker和递归 failure lookup。
8. 让 producer/frame/current manifest 持久化 owner Module 与 literal Vsn；在每个 frame MFA 调用前经过独立 owner-resolution seam，并生成 sanitized current-to-downstream summaries。
9. 实现 inspect matcher 的全量 preflight 与 fail 的 CcRef-deduped sorted commit；实现 protected unavailable continuation 与 unprotected old tuple 的不同 fallback。
10. 执行 focused self-review：搜索并消除重复 record、完整 `sf_cc` terminal、直接 action pattern、take 后 rollback/retry、Context/Env 泄露和错误的 sf-migrate ownership。

## Focused validation

- `rebar3 compile`
- `rebar3 ct --suite async_sf_SUITE`
- `rebar3 ct --suite async_t_SUITE`
- `rebar3 eunit`
- `rebar3 xref`
- 对新增/退役 symbols 做限定 `rg` 审计，证明没有平行 final owner 与旧 reader bypass。

## Blocking self-validation

- Commands: `rebar3 compile`; `rebar3 eunit`; `rebar3 xref`; targeted symbol/owner `rg` audit。

## Deferred downstream validation

- Commands: `rebar3 ct --suite async_sf_SUITE`; `rebar3 ct --suite async_t_SUITE`
- Owner: `task-test-1`
- Expected transitional failures: `async_sf_SUITE` old-fixture cases `same_monad_lowering:408`, `frame_completion:162`, `nested_composition:415`, `capture_compatibility:239`, `structural_failures` asserting the retired embedded `sf_cc` descriptor / single callback-ref shape; `async_t_SUITE` cases `test_async_t_progn_par:495`, `test_async_t_pmap_multiple_messages:628`, `test_async_t_pmap:736`, `test_async_t_pmap_with_acc:797` asserting old ordinary legacy accumulator/list values instead of `undefined` after strict R2/R9 orphan cleanup.
- Closure gate: `task-test-1` must update only its owned fixtures, expectations, and harnesses, run focused and full validations, and obtain a passed task-test report/review before initiative completion; all nine failures remain open and none is recorded as a passing coding validation.

## Required evidence

- 每个 owned path 的变更摘要与对应 requirement/evidence ID。
- immediate/suspended 路径的 State/CcRef before-after 说明；cleanup order 与 consumed-on-failure 证据。
- ordinary runner/wait normal behavior 未变的自测结果。
- shared decoder 的完整 reader 搜索、退役 symbols/no-match 和 scope propagation 调用链。
- 所有 validation 命令、退出码、目标 commit/tree 与已知未运行项。

## Observable completion

- 产品代码可编译且现有 sf-do/ordinary targeted suites 通过。
- public/new internal API、tuple shapes、return/error semantics 与 R1-R19 一致。
- source 中只有一个 final completion owner；frame terminal 不包含 callback/Context；old tuple 没有 fabricated CcRef。
- user callback 永远在 owned lifecycle 清理后运行，且任何 post-take failure 都不造成二次调用。
- worker 返回一个完整 candidate 与可供独立 test task 验证的精确 handoff。

## Stop conditions

- 规范对 observable behavior 需要新增选择；
- 必须修改非 owned product/OpenSpec/test path；
- 无法窄分类 unavailable old continuation 而不吞掉 business exception；
- 无法在不实现 sf-migrate policy 的情况下建立 owner-resolution seam；
- baseline 在 immutable target 上无法复现，或发现相关未提交 delta。

## Handoff

返回 `Status`、candidate tree/commit、owned path diff、capability disposition、focused validation、未运行检查、风险与对 `task-test-1` 的 fixture/API handoff。不得 stage、commit、修改 plan/status/checklist 或自行开始 test parent。
