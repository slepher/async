# Test Task Review

Status: changes_required

Task: task-test-1

Reviewed commit: `bc28febe142000025abaf59c71c8b41ab6cffe99`

Previous reviewed commit: `bc28febe142000025abaf59c71c8b41ab6cffe99`

## Evidence

- report-2 的 correction delta 新增 `test/async_sf_cc_SUITE.erl` 与 `test/async_sf_cc_macro_SUITE.erl`，并保留上一轮 `test/async_sf_SUITE.erl`、`test/async_t_SUITE.erl` 修正；未修改 `src/`、`include/`、`rebar.config`、OpenSpec、workflow/status/contracts。
- 新 runtime suite 为 9/10，新 macro suite 为 3/3；原 deferred suites 保持 `async_sf_SUITE` 10/10、`async_t_SUITE` 25/25。full CT 为 63 passed、1 failed。
- coverage map 已直接建立 R1–R19；真实 old-three-tuple fixture 位于合同授权的 runtime suite seam。唯一失败是 R15 `old_three_tuple_unavailable`，因此上一轮全部 test-owned omissions 已关闭，没有剩余 test fixture/harness correction。
- `test/async_sf_cc_SUITE.erl:190` expected `{legacy_outcome, purged_reply, PurgedClean}`，actual `{state,#{},[]}`。report-2 进一步证明 runtime 已产生 `{{legacy_outcome, purged_reply, CleanState}, CleanState}`，但 public `async_m:handle_info/3` 丢弃 outcome 并只返回 `CleanState`。

## Findings

1. Product-owned R15 return semantics 未满足。commit/report evidence 是 `test/async_sf_cc_SUITE.erl:190` 的唯一失败以及 report-2 的 `Product finding`：旧三元 callback 在 anonymous fun 所属旧代码被 purge 后，底层已正确生成 `{legacy_outcome, purged_reply, CleanState}`，public `async_m:handle_info/3` 却把 outcome tag 丢掉并暴露 `CleanState`。合同依据是 R15 必须把 unavailable legacy callback 的 outcome 作为可观察结果保留下来，同时完成 cleanup。`task-coding-1` 应仅在其现有 owned product path `src/async_m.erl` 的 `handle_info/3` return boundary 修正该丢弃行为：按底层结果顺序提取并返回 `{legacy_outcome, Outcome, CleanState}`，继续以同一个 `CleanState` 保持 callbacks/store cleanup，不改变 resumable old tuple、ordinary message/reply/timeout/DOWN、business exception、nested `undef`、exactly-once 或其他现有 test semantics。focused validation 为 `rebar3 ct --suite async_sf_cc_SUITE` 10/10；随后 macro suite 3/3、`async_sf_SUITE` 10/10、`async_t_SUITE` 25/25 与 full `rebar3 ct` 64/64。observable closure 是 line 190 收到带 `legacy_outcome` tag 的 expected value，而不是放宽或修改测试 expectation。

## Verdict

changes_required

Test-owned correction 已完成，但 task-test-1 仍有一个确定的 product-owned R15 failure；runtime suite 9/10 与 full CT 63/64 不能判为 passed。

## Changes

Correction owner: coding

Test correction: none

Affected coding tasks: `task-coding-1`

仅按上述 guidance 修正 `task-coding-1` 的现有 owned `src/async_m.erl` return boundary，并保留 report-2 的全部测试及其 assertions。产品 correction 通过其 task-scoped coding reviewer 后，重新运行并提交新的 test report。

## Routing

- Task: task-test-1.md
- Report: task-test-1-report-2.md
- Reviewed checkpoint: bc28febe142000025abaf59c71c8b41ab6cffe99
- Correction owner: coding
- Test correction: none
- Affected coding tasks: task-coding-1.md
- Verdict: changes_required

## Caveats

- `rebar3 dialyzer` 的 140 个 product/dependency boundary warnings 不属于 test correction，本轮不把它们判为 passed。
- OpenSpec CLI 不可用，因此 strict validation 未通过/未执行成功；不作为 test correction，也不能声称 passed。
- OTP 21–29 images/matrix 不可用；兼容性矩阵未验证，不作为 test correction，也不能声称 passed。
