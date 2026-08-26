# Test Task Report

Status: cannot_resolve

Task: task-test-1

Candidate: `bc28febe142000025abaf59c71c8b41ab6cffe99`

## Routing

- Task: task-test-1.md
- Report: task-test-1-report-2.md
- Scope: parent-complete
- Checkpoint: bc28febe142000025abaf59c71c8b41ab6cffe99
- Target: bc28febe142000025abaf59c71c8b41ab6cffe99
- Command: rebar3 ct --suite async_sf_cc_SUITE; rebar3 ct --suite async_sf_cc_macro_SUITE; rebar3 ct --suite async_sf_SUITE; rebar3 ct --suite async_t_SUITE; rebar3 ct
- Result: failed

## Result

- `task-test-1-review-1` 的 test-owned coverage omission 已补齐。
- 新增 runtime suite：9/10 passed；新增 macro suite：3/3 passed。
- 原 9 个 deferred failures 保持关闭：`async_sf_SUITE` 10/10、`async_t_SUITE` 25/25。
- 唯一剩余 CT failure 是新测试发现的 product-owned R15 defect；无剩余 test-owned harness failure。

## Changes

- 新增 `test/async_sf_cc_SUITE.erl`（394 lines），覆盖 runner/mode/context、CcRef store/scope、cleanup/exactly-once、message/reply/timeout/DOWN、frame terminal、old tuple、matcher privacy/atomicity、active fail dedupe 与 literal Vsn。
- 新增 `test/async_sf_cc_macro_SUITE.erl`（79 lines），覆盖 macro source/arity diagnostics。
- 保留上一轮 owned fixture/expectation 更新：`test/async_sf_SUITE.erl`、`test/async_t_SUITE.erl`。
- 未修改 `test/async_histories_SUITE.erl`，真实 old-three-tuple fixture 位于合同授权的新 runtime suite seam。
- 未修改 `src/`、`include/`、`rebar.config`、OpenSpec、workflow/status/contracts；未 stage、commit 或 merge。

## Product finding

- `test/async_sf_cc_SUITE.erl:190` 的 `old_three_tuple_unavailable` 使用真实旧状态：callbacks store 保存 `{callback, OldAnonymousFun, AccRef}` 三元 tuple，动态加载 v1 anonymous fun 后 purge old code。
- Runtime 正确产生 `{{legacy_outcome, purged_reply, CleanState}, CleanState}`，但 public `async_m:handle_info/3` 实际只返回 `CleanState = {state,#{},[]}`，丢弃 `{legacy_outcome, ...}` tag。
- Expected pattern：`{legacy_outcome, purged_reply, PurgedClean}`；actual：`{state,#{},[]}`。
- This is a product-owned R15 failure. Test did not modify or relax the expectation.

## Validation

| Command | Exit | Result |
|---|---:|---|
| `rebar3 ct --suite async_sf_cc_SUITE` (final) | 1 | 9/10 passed；仅 `old_three_tuple_unavailable` R15 product failure |
| `rebar3 ct --suite async_sf_cc_macro_SUITE` (final) | 0 | 3/3 passed |
| `rebar3 ct --suite async_sf_SUITE` | 0 | 10/10 passed |
| `rebar3 ct --suite async_t_SUITE` | 0 | 25/25 passed |
| `rebar3 compile` | 0 | passed |
| `rebar3 eunit` | 0 | 12/12；2 条既有 unnamed sf-do warnings |
| `rebar3 xref` | 0 | passed |
| `rebar3 ct` | 1 | 63 passed，1 failed；仅上述 R15 failure |
| `git diff --check` | 0 | passed |
| `workflow contract validator` | 0 | passed |

Earlier environment limits remain: `rebar3 dialyzer` exit 1 with 140 product/dependency warnings; `npx openspec validate sf-cc --strict` exit 1 because executable is unavailable; `rebar3 docker_ci run` exit 1 because `erlang:21` image is absent. OTP 21–29 matrix is unavailable and no claim of pass is made.

## Coverage map

- R1: `runner_modes_and_context` plus ordinary focused suites.
- R2: `pending_store_and_scopes` plus existing ordinary wait/runner cases.
- R3: macro suite three cases.
- R4–R5: `runner_modes_and_context`.
- R6–R8: `pending_store_and_scopes`, frame case, CcRef router/unhandled and cleanup cases.
- R9–R10: `completion_cleanup_exactly_once`, duplicate completion and invocation counter.
- R11–R13: `message_reply_timeout_down`, corrupted frame completion and CcRef-only terminal.
- R14: `old_three_tuple_resumable` and unprotected inspect summary.
- R15: `old_three_tuple_unavailable` (currently exposes the product failure above) plus nested `undef`/business exception negative classifiers.
- R16–R17: matcher privacy and atomicity cases.
- R18: active-fail dedupe and retained unprotected callback.
- R19: literal Vsn preservation.

## Closure

All nine deferred fixture/expectation failures and all test-owned coverage omissions are closed. Task-test observable completion remains open solely because the product-owned R15 failure makes the runtime suite 9/10 and full CT 63 passed/1 failed. This report does not claim full task completion, Dialyzer, OpenSpec, or OTP matrix passed.
