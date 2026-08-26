# Coding Final Correction Review

Status: complete

Task: task-coding-1

Reviewed commit: 10d85168c0f315b5f86ed6f35f501581bea8199f

Previous reviewed commit: 7fc7db0896a24556bd83c0b59ae60be9e35d0ba0

Evidence:

- immutable correction commit 的唯一 parent 为 `7fc7db0896a24556bd83c0b59ae60be9e35d0ba0`，delta 仅修改 task-owned `src/async_sf_cc.erl`，没有 `test/` 或其他路径变化。
- review-2 指出的 snapshot virtualization 已完整删除：`AccSnapshot` transport、`accumulator_snapshot/3`、`with_accumulator_snapshot/4`、`local_callbacks/3` 及对 `async_r_t` transformer layout 的重复构造均已不存在；async/legacy returned effect 恢复使用标准 `async_r_t` getter/setter，因此 callback-created `put_local/get_local` 可正常 round-trip。
- clean-state sequencing 仍通过 `bind(async_r_t:do_put_state(CleanState, MR), fun(_) -> invoke(...) end, MR)` 保持 lazy invoke；`cleanup_acc_ref/2` 与 post-invoke cleanup owner 没有恢复。direct-state 分支也先提交 clean state再 invoke。
- coding worker 的 focused evidence 报告 `put_local(new_value)` 后 `get_local()` 返回 `new_value` 且 final state 包含 callback-created effect；shared accumulator 在 unrelated callback 引用时保留；callback crash 后 consumed/no retry；ordinary/direct result shape 保持。mechanical evidence 为 compile exit 0、`async_t_SUITE` 25/25、EUnit 12/12、xref exit 0、`git diff --check` exit 0。
- final omission pass 重新检查了累计候选的 decoder ownership、AccRef/CcRef scope、descriptor/store/completion owner、cleanup/take/invoke、runner/wait normalization、terminal 与 Module/Vsn seam、inspect/fail privacy/dedupe、old tuple fallback、直接 API return shapes、重复 capability/wrapper/state noise及 sf-migrate policy；除下述 legacy accumulator 特例外，没有发现新的独立 omission。

Findings:

- `F1`（仍 blocking，R2/R9）：commit `10d85168c0f315b5f86ed6f35f501581bea8199f` 在 `src/async_sf_cc.erl:129-131` 将 `descriptor_acc_refs(SfCc, ActiveAccRef)` 作为额外 surviving refs 传入 orphan cleanup，而 `src/async_sf_cc.erl:171-172` 对 `#{mode := legacy}` 无条件返回 `[ActiveAccRef]`。ordinary `run_cc`、`run_with_cc` 与 wait 均由 `async_t:run_with_descriptor/3` 使用 legacy descriptor，因此即使 CcRef-owned action callbacks 已全部 take/删除、也没有 unrelated callback 引用该 AccRef，旧 accumulator 仍会保留在 `CleanState` 并进入 user final callback。该行为与 correction evidence 所称“orphan old ActiveAcc cleanup retained”不符，也违反 R2 将 ordinary runners 纳入同一 CcRef lifecycle 后，R9 要求 accumulator cleanup 在 invoke 前完成的顺序。`descriptor_acc_refs/2` 还是没有真实 surviving descriptor reference 支撑的特殊 state channel。

Verdict: changes_required

Changes:

- Correction owner: coding。仅修改 task-owned `src/async_sf_cc.erl`：删除 `descriptor_acc_refs/2` 和三参数 `remove_orphan_accumulators/3` 特例，让 `prepare/5` 对 async_r_t、direct_state、legacy 三种 descriptor 一致调用现有两参数 orphan cleanup；survival 只能由 callbacks store 中实际仍存在、经 shared decoder 得到同一 AccRef 的 action callback 证明。继续保留 clean-state `bind` 后 lazy invoke；callback effect 从已清理的正常 scope 开始，随后自行 `put_local` 创建的新值由标准 `async_r_t` semantics 保存，不恢复旧 orphan accumulator。
- Focused closure：用 ordinary `run_cc`/wait 的 legacy descriptor 建立 suspended action，并在 completion 前放入 ActiveAccRef accumulator；final callback 的第一检查点必须看不到 CcRef entry、owned action/resource 与旧 orphan accumulator，随后同一 callback effect 的 `put_local(new_value)`/`get_local()` 必须得到 `new_value`。另保留 unrelated callback 引用同 AccRef 时 accumulator survives、callback crash consumed/no retry、ordinary/direct result shape 的 probes；重跑 compile、`async_t_SUITE`、EUnit、xref 与 `git diff --check`。

Caveats:

- impossible evidence-boundary repair 仍然有效：5 个已知旧 `async_sf_SUITE` terminal fixture 只验证已退役的内嵌完整 `sf_cc` / 单 callback-ref shape，继续属于 `task-test-1` 已拥有的下游 migration gate；本 review 未据此要求 coding worker 修改 `test/`。
- final omission pass 未发现 F1 之外的新产品 finding；但 F1 尚未关闭，候选不能 `passed` 或进入 merge。
- 本 review 未修改任何产品或 test path，只写入本 immutable review artifact；未 stage、commit，也未修改 status、ledger、plan、lanes、OpenSpec 或 checklist。
