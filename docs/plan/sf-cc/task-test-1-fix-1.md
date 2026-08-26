# Derived Coding Fix Contract

- Task: task-test-1-fix-1
- Fix kind: Coding fix discovered by integration
- Trigger source: task-test-1
- Trigger report: task-test-1-report-2.md
- Trigger review: task-test-1-review-2.md
- Covered bugs: R15-public-legacy-outcome-tag
- Affected coding task: task-coding-1.md
- Inherited mode: hard
- Inherited lane: coding
- Failure observed target: bc28febe142000025abaf59c71c8b41ab6cffe99
- Integration checkpoint: bc28febe142000025abaf59c71c8b41ab6cffe99
- Fix base: bc28febe142000025abaf59c71c8b41ab6cffe99
- Depends on: task-coding-1 accepted merge; task-test-1-report-2.md; task-test-1-review-2.md
- Resume gate: rerun task-test-1

## Bounded Goal

修复 R15 的 public boundary：当 purged old anonymous callback 触发 runtime `{legacy_outcome, Outcome, CleanState}` 时，public `async_m:handle_info/3` 必须保留并返回该完整 tag，不得只投影或吞掉 outcome；返回中的 `CleanState` 必须与 runtime 已完成旧 callback/accumulator cleanup 后的 state 相同。

## Failure Binding

- failure 已在 integration checkpoint `bc28febe142000025abaf59c71c8b41ab6cffe99` 上重现；failure-observed target 与 fix base 均为该 immutable commit。
- integration report/review 将 defect 精确绑定为 `R15-public-legacy-outcome-tag`，affected coding authority 为 accepted `task-coding-1.md`。
- 本 fix 只修复 public adapter 对既有 runtime tag 的保留与返回，不重新解释 unavailable classification，也不改变 test expectation。

## Owned Paths

- `src/async_m.erl`

不得修改 `test/`、`include/`、`rebar.config`、其他 `src/` path、OpenSpec、accepted task contracts、plan、lanes、status、ledger 或 checklist。若正确修复需要任何其他产品路径或测试修正，立即停止并报告精确路径与原因。

## Exact Correction

- 在 `src/async_m.erl` 的 public `handle_info/3` boundary 保留 runtime 返回的 `{legacy_outcome, Outcome, CleanState}`，并把该完整 tagged value 返回给 public caller。
- tag 内的 `Outcome` 必须是 runtime 提供的原 action outcome；`CleanState` 必须原样沿用 runtime cleanup 后的 state，不得重建、二次清理或替换。
- 不得把 tagged fallback 投影成裸 State、裸 Outcome、`{Outcome, State}` 或其他新 wrapper；不得新增第二个 legacy classifier 或 completion owner。

## Preserved Invariants

- resumable old three-tuple callback 继续走原 continuation，不产生 fabricated CcRef，也不错误返回 `legacy_outcome`。
- ordinary message/reply/timeout/DOWN 的 public `handle_info/3` return shape 与行为保持不变；只有 runtime 已明确产生 R15 tag 的 unavailable-old-callback path 保留 tagged return。
- ordinary business exception 与 nested `undef` 继续传播或按既有路径处理，不得被重新分类为 unavailable old continuation。
- callback entry、owned resources 与 orphan accumulator 的 cleanup 仍只由既有 completion/runtime owner 执行；public adapter 不重复 cleanup。
- take 后 failure、late/duplicate event 与 exactly-once/consumed-on-failure 语义不变；不得恢复、重试或二次调用 old callback。
- 不改变 R1–R14、R16–R19、ordinary runner/wait、sf frame、inspection/fail 或 sf-migrate boundary。

## Ordered Work

1. 在 fix base 上定位 `async_m:handle_info/3` 的 public adapter return projection，以及 runtime R15 tag 到该 boundary 的现有直达调用链。
2. 只在 `src/async_m.erl` 做最小 adapter correction：识别既有 runtime tagged return并原样保留；普通 return 继续使用现有 projection。
3. 审计该 module，确认没有复制 unavailable classifier、cleanup、CcRef owner或 runtime result normalization。
4. 执行 focused case、blocking product validation及 task-test-1 full rerun；记录 exact candidate commit/tree、命令、exit code与 open failures。

## Focused Self-Validation

按顺序执行：

1. `rebar3 ct --suite async_sf_cc_SUITE --case old_three_tuple_unavailable`
2. `rebar3 compile`
3. `rebar3 eunit`
4. `rebar3 xref`
5. 按 accepted task-test-1 contract 完整重跑 task-test-1 focused/full validations。

Focused case 必须同时证明：purged old anonymous callback 的 public result 为精确 `{legacy_outcome, Outcome, CleanState}`；CleanState 已完成原 cleanup；resumable old tuple 不产生该 tag；business exception/nested `undef` 未被吞掉；重复 event 不造成二次调用。

## Observable Completion

- public `async_m:handle_info/3` 对 integration repro 返回精确 R15 tagged value及同一 CleanState。
- 所有 preserved invariants 成立，diff 只包含 `src/async_m.erl`。
- focused case、compile、EUnit、xref 通过；task-test-1 full rerun 生成新的 report，并由 paired reviewer 作出结论。

## Stop Conditions

- 修复需要 `src/async_m.erl` 之外的产品路径或任何 test path；
- runtime 实际未提供可无损保留的 R15 tag，必须新增或改变 unavailable classification；
- ordinary `handle_info/3` return contract 与 tagged fallback 之间需要新的 observable behavior choice；
- 出现 report/review 未绑定的新 failure/signature、baseline 无法在 fix base 重现，或发现相关未提交 delta。

## Handoff

fix worker 返回 `Status`、candidate commit/tree、单路径 diff、R15 public return before/after、preserved-invariant evidence、全部 validation 命令与 exit code、未运行项及风险。不得 stage/commit workflow artifacts、修改 status，或自行修改/重跑测试源码。

coding fix checkpoint 通过 reviewer 后，由 dispatcher 持久化并合并；随后按 Resume gate 重新启动 task-test-1。initiative 只有在新的 task-test report/review 通过后才能继续完成。
