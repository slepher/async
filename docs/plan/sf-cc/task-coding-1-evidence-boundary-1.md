# Coding Evidence-Boundary Review

Status: complete

Task: task-coding-1

Reviewed command/failures:

- Candidate checkpoint: `34e3a7460939131dc32f85e84838571ca9be015e`。
- `rebar3 ct --suite async_sf_SUITE`：以下 5 个 transitional failures 保持 open，不能记为 pass：`same_monad_lowering:408`、`frame_completion:162`、`nested_composition:415`、`capture_compatibility:239`、`structural_failures`。其精确签名均为旧 fixture 仍断言已退役的 embedded `sf_cc` descriptor / single callback-ref shape。
- `rebar3 ct --suite async_t_SUITE`：以下 4 个 transitional failures 保持 open，不能记为 pass：`test_async_t_progn_par:495`、`test_async_t_pmap_multiple_messages:628`、`test_async_t_pmap:736`、`test_async_t_pmap_with_acc:797`。其精确签名均为 strict R2/R9 orphan cleanup 后，旧 ordinary legacy accumulator/list expectation 的 expected old accumulator/list 与 actual `undefined` 不一致。
- supplied worker evidence 将失败全集限定为上述 9 个 case/signature；candidate 的产品 probes、compile、EUnit、xref 通过，但这些证据不替代上述两个 Common Test 命令的下游 closure，也不构成 coding acceptance。

Downstream owner: task-test-1

- `task-test-1` 已拥有 `test/async_sf_SUITE.erl`、`test/async_t_SUITE.erl` 及其 contract 命名的新 fixtures/harnesses，可更新旧 terminal expectations、ordinary accumulator expectations、fixtures 与 harness。
- `task-test-1` 不得修改 `src/`、`include/` 或 `rebar.config`；本 boundary repair 不转移产品 ownership，也不授权 test task 作产品行为决定。

Proposed contract delta:

- dispatcher 可在 accepted `task-coding-1.md` 中只追加并序列化以下精确 clause；除此之外不得修改 contract：

```markdown
## Deferred downstream validation

- Candidate checkpoint: `34e3a7460939131dc32f85e84838571ca9be015e`。
- Deferred command: `rebar3 ct --suite async_sf_SUITE`。
  - Open transitional failures: `same_monad_lowering:408`、`frame_completion:162`、`nested_composition:415`、`capture_compatibility:239`、`structural_failures`。
  - Deferred signature: old fixtures assert the retired embedded `sf_cc` descriptor / single callback-ref shape。
- Deferred command: `rebar3 ct --suite async_t_SUITE`。
  - Open transitional failures: `test_async_t_progn_par:495`、`test_async_t_pmap_multiple_messages:628`、`test_async_t_pmap:736`、`test_async_t_pmap_with_acc:797`。
  - Deferred signature: after strict R2/R9 orphan cleanup, old ordinary legacy accumulator/list expectations report expected old accumulator/list versus actual `undefined`。
- Owner: `task-test-1`。
- Boundary: `task-test-1` may update only its owned fixtures, expectations, and harnesses; it may not modify `src/`, `include/`, or `rebar.config` and may not choose or alter product behavior。
- Closure gate: `task-test-1` must update the owned fixtures/expectations/harness, run its focused and full validations, and obtain a passed task-test report/review before initiative completion. All nine failures remain open until that report/review passes; none is recorded as a passing coding validation.
```

- 该 delta 只重分类 validation evidence 的阶段边界；保持 product behavior、owned paths、mode、lane、dependencies、`Covers` 与 final acceptance 不变。coding lane 仍须先完成 accepted merge，之后才可按现有 lane gate 启动 `task-test-1`。

Closure gate:

- dispatcher 应先应用上述逐字 contract delta，并完成 workflow/contract validation；这一步只修复 evidence boundary，不接受 coding candidate。
- coding candidate 通过其独立 checkpoint/review/merge gate 后，`task-test-1` 才能开始。
- `task-test-1` 必须在 owned test paths 内关闭全部 9 个精确 case/signature，执行 contract 要求的 focused 与 full validations，并提交 task-test report；paired reviewer 必须返回 `passed`。
- 在 task-test report/review 通过前，两个 deferred commands 与 9 个 failures 均保持 open，initiative 不得完成。

Verdict: passed

Caveats:

- 本 `passed` 仅授权 dispatcher 应用上面的精确 contract delta 并验证 workflow；它不接受 commit `34e3a7460939131dc32f85e84838571ca9be015e`、不声明任何 Common Test command 已通过，也不解除 coding 自身的 checkpoint/review/merge gate。
- accepted `task-coding-1.md` 当前没有 `## Deferred downstream validation` heading；本 artifact 审核的唯一修复正是新增上述显式 clause，不是覆盖、删除或放宽原 validation/acceptance 内容。
- 5 个 `async_sf_SUITE` cases 延续用户已授权的旧 terminal fixture migration 分类；4 个 `async_t_SUITE` cases 只因其命令、精确 failure signature 与 `task-test-1` owned path 完整匹配而被纳入同一 downstream gate。若后续出现任何新 case、不同 signature、非 owned path 或新的 behavior decision，本授权不适用。
- 本 review 未读取或修改产品/test code，未执行 simplicity audit；未修改 contract、status、Git、plan、lanes 或 checklist，只写入本 immutable evidence-boundary artifact。
