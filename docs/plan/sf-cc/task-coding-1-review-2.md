# Coding Correction Review

Status: complete

Task: task-coding-1

Reviewed commit: 7fc7db0896a24556bd83c0b59ae60be9e35d0ba0

Previous reviewed commit: 30d29c3d7ecc4da96e1052b8066c43a4298aee76

Evidence:

- immutable correction commit 的唯一 parent 为 `30d29c3d7ecc4da96e1052b8066c43a4298aee76`，delta 只修改 task-owned `src/async_sf_cc.erl`。
- F1 的 cleanup/ordering 部分已关闭：`prepare/5` 把 `ActiveAccRef` 与 removed AccRefs 一并交给 `remove_orphan_accumulators/2`，仍被 unrelated callback 引用的 shared accumulator 会保留；`invoke_prepared/6` 通过 `bind(async_r_t:do_put_state(...), fun(_) -> ... end, MR)` 先提交 clean state，再延迟构造和调用 user callback；旧 `cleanup_acc_ref/2` symbol 与 post-invoke owner 路径已删除。
- coding worker 的 focused evidence 报告 callback 第一检查点看到 clean callbacks、unrelated surviving callback 会保留 shared accumulator、callback crash 后 CcRef consumed 且不 retry、ordinary result shape 不变。mechanical evidence 为 compile exit 0、`async_t_SUITE` 25/25、EUnit 12/12、xref exit 0、`git diff --check` exit 0。
- 5 个已知旧 `async_sf_SUITE` terminal fixture 的 `task-test-1` 下游 migration-gate 分类保持不变；本 correction review 未以其要求 coding worker 修改 `test/`。

Findings:

- `F1`（仍 blocking，callback-created effects 未保留）：commit `7fc7db0896a24556bd83c0b59ae60be9e35d0ba0` 在 `src/async_sf_cc.erl:229-244` 新增 `with_accumulator_snapshot/4` 与 `local_callbacks/3`。当 old ActiveAccRef 已按 R9 清理时，localized getter 每次都把旧 snapshot `Value` 注入 callbacks，而 localized setter 每次都把同一 AccRef 删除。因此 callback effect 执行 `put_local(NewValue)` 后，该写入在 setter 边界立即丢失，下一次 `get_local()` 又得到旧 snapshot；callback 新建的 local state/effect 既不能在同一 effect 中 round-trip，也不能进入其返回 state。这违反本 correction 的显式 closure“preserve callback-created effects”，并会改变 ordinary async/legacy callback 的可观察 state behavior。结构上，`local_callbacks/3` 还在 `async_sf_cc` 手工重建 `async_r_t` 的 `state_t + 三层 reader_t` representation，形成第二个 transformer-layout owner；这是同一行为缺陷的脆弱来源。

Verdict: changes_required

Changes:

- Correction owner: coding。仅需在 task-owned `src/async_sf_cc.erl` 删除 `AccSnapshot` transport、`accumulator_snapshot/3`、`with_accumulator_snapshot/4` 与 `local_callbacks/3`：`prepare/5` 仍在 invoke 前清除无 surviving reference 的 old ActiveAccRef，并返回 clean state；`invoke_prepared` 仍先通过 `bind` 提交 clean state，再 lazy `invoke` async/legacy callback，但其返回 effect 应直接使用正常 `async_r_t` callback/state owner，不再虚拟恢复已清理的旧 accumulator。这样 callback 起点保持 clean，callback 随后自行 `put_local` 创建的新值可由正常 getter/setter round-trip，而 consumed CcRef scope 继续为 `undefined`。保留 unrelated surviving callback 所引用的 accumulator、direct `/2|3` semantics、old tuple fallback、message behavior 与 callback-crash consumed-on-failure。
- Focused closure：除已有 clean-first/crash/shared-accumulator probes 外，增加一个 callback effect probe，在 final callback 返回的 effect 内依次执行 `put_local(NewValue)`、`get_local()` 并检查结果为 `NewValue`，同时最终 state 反映该 callback-created effect；确认 callback 的第一次读取不再看见已清理的旧 orphan accumulator。随后重跑 compile、`async_t_SUITE`、EUnit、xref 与 `git diff --check`。

Caveats:

- 因 F1 尚未完整关闭，本轮不执行可产生 `passed` 的最终 omission pass；此前已接受的 callback decoder、CcRef/AccRef scope separation、terminal/owner/Vsn、privacy/dedupe、old tuple fallback、ordinary return shape及 no-sf-migrate 结论未被本 delta 推翻。
- 本 review 未修改任何产品或 test path，只写入本 immutable review artifact；未 stage、commit，也未修改 status、ledger、plan、lanes、OpenSpec 或 checklist。
