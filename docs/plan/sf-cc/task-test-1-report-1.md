# Test Task Report

Status: cannot_resolve

Task: task-test-1

Candidate: `bc28febe142000025abaf59c71c8b41ab6cffe99` (tree `8f682a2ee98c5ad7d4db9056f1ca1f7fb6545b78`)

## Routing

- Task: task-test-1.md
- Report: task-test-1-report-1.md
- Scope: parent-complete
- Checkpoint: bc28febe142000025abaf59c71c8b41ab6cffe99
- Target: bc28febe142000025abaf59c71c8b41ab6cffe99
- Command: rebar3 ct --suite async_sf_SUITE; rebar3 ct --suite async_t_SUITE; rebar3 ct
- Result: failed

## Result

- 在 immutable candidate 上关闭全部 9 个 deferred failures。
- focused suites 最终通过：`async_sf_SUITE` 10/10，`async_t_SUITE` 25/25。
- full CT、compile、EUnit、xref 通过。
- 合同整体仍不能判为 passed：Dialyzer 有 140 个非 test-owned warnings；OpenSpec CLI 不可用；OTP Docker images 不存在；合同要求的真实 old-three-tuple fixture 和 `async_sf_cc_SUITE` 不存在于当前 bounded scope。

## Candidate and changes

- HEAD 在开始与结束时均为 `bc28febe142000025abaf59c71c8b41ab6cffe99`。
- 仅修改 owned paths：`test/async_sf_SUITE.erl`、`test/async_t_SUITE.erl`。
- `async_sf_SUITE.erl`：terminal expectation 更新为公开 `sf_cc_ref`；通过 `async_util:callback_value_kind/1` 选择 action callback ref；standalone/structural fixtures 注册 CcRef 并携带新 frame fields；synthetic missing-target frame 使用合法 frame shape 与不存在 owner module。
- `async_t_SUITE.erl`：四个 ordinary final callback expectation 更新为 strict orphan cleanup 后的 `undefined`，移除对已清理 accumulator 的 list/tuple 解构。
- 未修改 `src/`、`include/`、`rebar.config`、OpenSpec、workflow/status/contracts；未 stage、commit 或 merge。

## Validation

| Command | Exit | Result |
|---|---:|---|
| `rebar3 ct --suite async_sf_SUITE` (baseline) | 1 | 精确 5 个 deferred failures |
| `rebar3 ct --suite async_t_SUITE` (baseline) | 1 | 精确 4 个 deferred failures |
| `rebar3 ct --suite async_sf_SUITE` (final) | 0 | 10/10 passed |
| `rebar3 ct --suite async_t_SUITE` (final) | 0 | 25/25 passed |
| `rebar3 compile` | 0 | passed |
| `rebar3 eunit` | 0 | 12/12；2 条既有 unnamed `sf_do` warnings |
| `rebar3 ct` | 0 | 51/51 passed |
| `rebar3 xref` | 0 | passed |
| `rebar3 dialyzer` | 1 | 140 warnings |
| `npx openspec validate sf-cc --strict` | 1 | npm could not determine executable |
| `rebar3 docker_ci run` | 1 | 缺少 `erlang:21` image；角色协议不授权 pull |
| workflow contract validator | 0 | passed |
| `git diff --check` | 0 | clean |

Common Test 原始报告入口：`_build/test/logs/index.html`；Dialyzer warnings：`_build/default/29.0.4.dialyzer_warnings`。

## Baseline and closure evidence

- `async_sf_SUITE` baseline 5 failures：`same_monad_lowering:408`、`frame_completion:162`、`nested_composition:415`、`capture_compatibility:239`、`structural_failures`。
- `async_t_SUITE` baseline 4 failures：`test_async_t_progn_par:495`、`test_async_t_pmap_multiple_messages:628`、`test_async_t_pmap:736`、`test_async_t_pmap_with_acc:797`。
- Intermediate fixture iterations produced only the expected synthetic frame/fixture failures; final reruns closed the exact nine cases and found no extra case/signature.

## Findings and limits

- Dialyzer failure is at product/dependency typing boundaries, including `src/async_sf_cc.erl:182,187` and `src/async_t.erl:439,922,1139`; no warning is in test-owned paths. Regression status is unknown and requires review classification.
- OpenSpec failure is tooling unavailable, not a schema finding.
- OTP 21–29 compatibility matrix is unavailable because required Docker images are absent; worker is not authorized to pull/install them.
- `async_histories_SUITE` still contains ordinary v1/v2/v3/v5 histories only and does not provide the required real old-three-tuple State fixture.
- `test/async_sf_cc_SUITE.erl` and an independent macro suite were not created; assignment was bounded to the nine deferred fixture/expectation cases.
- R1 is covered only by existing sf-do source/diagnostic tests; R2/R9 by the four ordinary cleanup cases; the remaining R3–R8 and R10–R19 document-driven matrix is not directly established by this report.

## Closure statement

The nine deferred failures are closed in their owned fixtures, but task-test-1 observable completion is not established. This report does not claim OpenSpec, Dialyzer, OTP matrix, missing old-three-tuple fixture, or full R1–R19 integration coverage passed.
