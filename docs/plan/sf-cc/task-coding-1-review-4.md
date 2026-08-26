# Coding Semantic Review

Status: complete

Task: task-coding-1

Reviewed commit: 34e3a7460939131dc32f85e84838571ca9be015e

Previous reviewed commit: 10d85168c0f315b5f86ed6f35f501581bea8199f

Evidence:

- immutable candidate `34e3a7460939131dc32f85e84838571ca9be015e` 的唯一 parent 为上一 reviewed commit `10d85168c0f315b5f86ed6f35f501581bea8199f`；相对 base `9691772e7e260e32cb5c7fd11c0cd51563b3530a` 的累计 diff 仍恰好只包含 accepted contract 的 12 条 owned paths，本轮 correction delta 仅为 `src/async_sf_cc.erl`。
- R1–R5：sf-prefixed macro/API、external-fun AST gate、descriptor Context、async/direct modes 与 ordinary API adapter 均由既定 owner 实现；direct `/2` 忽略 callback return，direct `/3` 使用 clean state 并通过 State tag/arity compatibility 校验。
- R6–R11：shared callback record 只有一个 header owner，三元/四元 tuple 统一经 `async_util:decode_callback/1`；AccRef/CcRef 使用独立 reader scope；CcRef entry 是唯一 final owner；message 保留 action/CcRef；reply/timeout/DOWN 进入统一 completion。当前 delta 删除 legacy descriptor 的虚假 surviving reference，`prepare/5` 现在对 ordinary、async_r_t 与 direct_state 一致执行 take、owned resource/action cleanup 及真实-reference-driven orphan accumulator cleanup，再提交 clean state并 lazy invoke。
- R12–R15：structural/directional failure 通过 decoded CcRef scope 委托 `async_sf_cc`；frame terminal 保持精确 `#{kind => sf_cc_ref, protocol => 1, mref => CcRef}`；old three-tuple decode 为 `undefined`，unavailable old continuation 的 fallback 保持 `{legacy_outcome, Outcome, CleanState}` 边界。
- R16–R19：inspection 仅返回 sanitized callback/frame metadata，matcher 在 mutation 前完整 preflight；active fail 按 CcRef 去重并排序 commit；transform/frame/manifest 保存 owner Module 与 literal Vsn，并逐 frame 独立 resolution；未引入 sf-migrate catalog、migrator、composition 或 downgrade policy。
- supplied validation evidence：candidate product probes、compile、EUnit、xref 通过；没有报告 deferred signatures 之外的新 failure 或非 owned product path。`rebar3 ct --suite async_sf_SUITE` 的 5 个旧 terminal cases 与 `rebar3 ct --suite async_t_SUITE` 的 4 个 strict-cleanup expectation cases 已由 accepted `## Deferred downstream validation` 精确契约化，仍为 open 且 Owner=`task-test-1`；本 review 未把两个 CT 命令记为 passed。
- fresh cumulative omission/simplicity pass 重新检查 decoder、scope、descriptor/store/completion owner、take/cleanup/invoke、runner/wait normalization、terminal/Module/Vsn、privacy/dedupe、old tuple、public return shape、wrapper/state channels 与 no-sf-migrate boundary；除下述 stale direct-state cleanup branch 外，没有发现新的 correctness、ownership 或 simplicity finding。

Findings:

- `S1`（blocking simplicity/ownership omission）：`src/async_sf_cc.erl:120-132` 的 `prepare/5` 已对所有 descriptor modes 把 `ActiveAccRef` 连同 owned action AccRefs 交给 `remove_orphan_accumulators/2`，并生成最终 `CleanState`。但 `src/async_sf_cc.erl:206-214` 的 direct-state 专用 `invoke_prepared/5` 随后再次 `ask` callbacks getter/setter、再次扫描同一 `ActiveAccRef`、再次构造等价 `StateForCallback` 后才提交 state；两次扫描之间没有 state transition，第二条 cleanup path 不保护任何独立 invariant，也不会产生不同结果。它是 review-1 前旧 cleanup ownership 在 correction 后留下的重复 capability与 callback/state wrapper，增加 direct completion 的 jump cost并形成两个 accumulator-cleanup位置，违反 accepted implementation 的 single completion owner 与完整 simplicity audit 的 linearity/duplicate-state-channel要求。

Verdict: changes_required

Changes:

- Correction owner: coding。仅修改 task-owned `src/async_sf_cc.erl`：删除 direct-state 专用 `invoke_prepared` clause 及其第二次 `ask`/`remove_orphan_accumulators`/callbacks write；让 async_r_t、legacy、direct_state 共用唯一线性路径 `bind(do_put_state(CleanState), fun(_) -> invoke(...) end)`。同时从 `invoke_prepared` interface 删除已经没有消费者的 `ActiveAccRef` 参数并更新 `complete/3`、`complete_state_result/4` 两个 callers。唯一 cleanup owner 保留在 `prepare/5`。
- 保持以下 invariants：真实 unrelated callback 引用同 AccRef 时 accumulator survives；无引用 old accumulator 在 callback 前消失；callback-created `put_local/get_local` 使用标准 `async_r_t` semantics；CcRef consumed/no retry；direct `/2|3` return/state semantics、ordinary result shape、message、old tuple fallback 与 CcRef scope reset 不变。
- Focused closure：运行 direct `/2` 与 `/3` probes，证明 callback 第一检查点接收 `prepare/5` 产生的 clean state、`/2` return 被忽略、`/3` compatible state 被保存且 incompatible state 仍报错；保留 orphan/shared-accumulator、callback-created local round-trip、crash consumed/no-retry probes，并重跑 accepted blocking self-validation（compile、EUnit、xref及其 contract probes）。两个 deferred CT commands 与 9 个 cases 继续保持 open，留给 `task-test-1` report/review closure。

Caveats:

- accepted `## Deferred downstream validation` 的 impossible evidence-boundary repair 仍有效：5 个 `async_sf_SUITE` 与 4 个 `async_t_SUITE` 精确 failures 不能单独导致本 coding review `changes_required`；本轮 verdict 仅由独立的产品实现 simplicity/ownership finding `S1` 导致。
- 未发现 deferred 列表之外的新 failure/signature、越界修改或新的 behavior choice；若 `S1` closure delta 不改变语义，仍属于当前 generation/task authority。
- 本 review 未修改任何产品、test、contract、status 或 Git state，只写入本 immutable review artifact；未 stage、commit，也未修改 plan、lanes、OpenSpec 或 checklist。
