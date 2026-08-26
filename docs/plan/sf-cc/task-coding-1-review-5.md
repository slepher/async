# Coding Semantic Review

Status: complete

Task: task-coding-1

Reviewed commit: 7b9c28a52b34f199ead7469d12c6124305853b35

Previous reviewed commit: 34e3a7460939131dc32f85e84838571ca9be015e

Evidence:

- immutable candidate `7b9c28a52b34f199ead7469d12c6124305853b35` 的唯一 parent 为 `34e3a7460939131dc32f85e84838571ca9be015e`；本轮 delta 仅修改 task-owned `src/async_sf_cc.erl`。相对 base `9691772e7e260e32cb5c7fd11c0cd51563b3530a` 的累计 diff 仍只包含 accepted contract 的 12 条 owned paths。
- review-4 `S1` 精确关闭：direct-state 专用 `invoke_prepared` cleanup branch 已删除，`ActiveAccRef` 已从 `invoke_prepared` interface 及两个 callers 移除；async_r_t、legacy、direct_state 现在共用唯一 `bind(do_put_state(CleanState), fun(_) -> invoke(...) end)` 路径。accumulator cleanup 只有 `prepare/5` 一个 owner，不再有第二次 callbacks ask/scan/write。
- direct `/2|3` semantic trace：clean state 在进入 `invoke/3` 前提交；direct `/2` 调用 callback 后忽略其 return 并返回原 Outcome；direct `/3` 从已提交的 clean state 取 State，只有 `same_type_state/2` 成功才写入 callback 返回 State，否则保持既有 `invalid_sf_cc_return` error。线性化 delta 没有改变这三项行为。
- lifecycle trace：`prepare/5` 先验证并 take CcRef entry，再清理同 CcRef owned timers/monitors/action callbacks；orphan accumulator 只有在 shared decoder 找不到 surviving callback reference 时删除，unrelated/shared AccRef 仍保留。callback 在 clean state 后 lazy invoke，returned effect 使用标准 `async_r_t` local semantics 且 CcRef scope 为 `undefined`，因此 callback-created `put_local/get_local` 可 round-trip；callback crash 的 failure path返回已 consumed 的 CleanState，不恢复或 retry CcRef。
- R1–R19 与 owner audit 保持成立：sf-prefixed external-fun macro/API、descriptor modes、shared callback decoder、独立 AccRef/CcRef scope、single completion owner、message non-consumption、structural failure delegation、精确 `sf_cc_ref` terminal、old tuple/legacy outcome、sanitized inspect、atomic matcher、CcRef-deduped fail、Module/literal Vsn resolution均在各自 owner；没有 duplicate record、retired invoker、parallel cleanup owner、terminal Context/Env leakage或 sf-migrate policy。
- 在 clean coding worktree、精确 candidate HEAD 上实际执行：`rebar3 compile` exit 0；`rebar3 eunit` exit 0，12 tests/0 failures；`rebar3 xref` exit 0；累计 `git diff --check` exit 0。执行后 tracked worktree 仍 clean。
- final omission/simplicity pass 重新检查 canonical return/diagnostic owner、exported symbol owner、ignored rich fields、重复 capability、wrapper/state/callback noise及 primary workflow jumps；`S1` 删除后未发现新的 product correctness、ownership 或 simplicity finding。

Findings:

- none。

Verdict: passed

Changes:

- none required。candidate 可进入 dispatcher 的 checkpoint persistence、产品 review acceptance 与 merge gate；本 review 不自行 stage、commit 或 merge。

Caveats:

- accepted `## Deferred downstream validation` 仍覆盖且仅覆盖以下 open failures：`async_sf_SUITE` 的 `same_monad_lowering:408`、`frame_completion:162`、`nested_composition:415`、`capture_compatibility:239`、`structural_failures`，以及 `async_t_SUITE` 的 `test_async_t_progn_par:495`、`test_async_t_pmap_multiple_messages:628`、`test_async_t_pmap:736`、`test_async_t_pmap_with_acc:797`。Owner 仍为 `task-test-1`。
- 本 `passed` 不声明 `rebar3 ct --suite async_sf_SUITE` 或 `rebar3 ct --suite async_t_SUITE` 已通过；9 个 failures 在 `task-test-1` 更新 owned fixtures/expectations/harness、完成 focused/full validations 并取得 task-test report/review `passed` 前始终保持 open。
- 本 review 未修改任何产品、test、contract、status 或 Git state，只写入本 immutable review artifact；未 stage、commit，也未修改 plan、lanes、OpenSpec 或 checklist。
