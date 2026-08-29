# Status:

changes_required

# Task:

task-coding-2

# Reviewed commit:

`cb03fb4c15cb55be438cc316308215bdbc5f7ca7`

# Evidence:

- 本轮是 correction-only semantic review。前次 immutable checkpoint 为 `aa5b0436514c044ae84bc00598153d45b26f4659`，本轮只审查 `aa5b0436514c044ae84bc00598153d45b26f4659..cb03fb4c15cb55be438cc316308215bdbc5f7ca7` 以及 review-1 Findings 1–8 的闭合；没有重开无关的 accepted code。
- coding worktree 的 HEAD 精确为 `cb03fb4c15cb55be438cc316308215bdbc5f7ca7`，`git status --short` 为空，且 `aa5b0436514c044ae84bc00598153d45b26f4659` 是其祖先。correction range 由 `640aa17` 与 `cb03fb4` 两个 commit 组成，只修改 `include/async_internal.hrl`、`src/async_r_t.erl`、`src/async_sf_cc.erl`、`src/async_sf_runtime.erl`、`src/async_sf_transform.erl`、`src/async_t.erl`、`src/async_util.erl`，共 535 insertions / 221 deletions；没有 test、OpenSpec、plan、status 或 lane 变更。
- supplied independent validation：`rebar3 compile`、`rebar3 xref`、`rebar3 eunit --module=async_sf_transform_tests`、`git diff --check`、`rebar3 dialyzer` 均 exit 0。`async_t_SUITE` 为 23 passed / 4 failed； supplied evidence 将四项精确限定为已接受的 AccRef legacy assertions，未报告新的 result、crash、timeout 或 resource-leak signature。
- correction delta 建立了 non-consuming message dispatcher、reply 后 AccRef cleanup、consumed-state error/`failed` branch、exact records/classifier、node ownership preflight、直接 availability query、exact manifest identity，以及显式 Context 单通道；`cb03fb4` 另把 cleanup 从 sequencing-only `>>` 改成保留原结果的 bind。
- final bounded omission pass 以 current action route、node dispatch、timer/monitor/AccRef cleanup、runtime invocation、manifest 与 explicit Context 为搜索种子。未发现 review-1 之外的新 capability owner；但 action 自身 reverse-index 未进入 preflight，且 monitor cleanup 尚未完全退役旧 owner，见 Finding 5。

# Findings:

1. **Finding 1 已闭合；本项可 merge。**
   - 证据：`src/async_t.erl:799-809` 的 outer runner 将 `{message, _}` 路由到 `async_sf_cc:dispatch_message/3`；`src/async_sf_runtime.erl:37-42` 对 frame remainder 的 parent runner采用同一路由；`src/async_sf_cc.erl:237-260` 只读取并执行 outer node，不调用 `take_node/4`。`route_action(Key, message, ...)` 在 `src/async_sf_cc.erl:77-89` 也返回原 callbacks。
   - 闭合判断：message 不再消费 ActionMRef、outer node 或 reverse index，reply/timeout/DOWN 仍走 consuming route，满足 message non-consumption 与 exactly-once 后续完成。
   - 剩余风险：旧 suite 仍需由 `task-test-2` 迁移 shape assertion；当前 correction delta 未显示新的 message result/resource signature。
   - 是否可 merge：是，本项不阻断。

2. **Finding 2 已闭合；本项可 merge。**
   - 证据：`src/async_sf_cc.erl:82-90` consume action 时不再提前删除 AccRef；`src/async_t.erl:938-958` 在原 AccRef scope 中执行 current continuation，并在成功结果产生后调用 `cleanup_accumulator/2`；`cb03fb4` 使用 bind 把原 `Result` 原样返回，而不是用 cleanup 的 `ok` 覆盖它。
   - 闭合判断：current continuation 能读取原 accumulator；cleanup 只在 action 已消费且没有 pending owner 时删除。`async_t_SUITE` 从 review-1 的公开 ordinary failures 恢复至 23 passed，剩余四项被 supplied evidence 限定为 accepted AccRef legacy assertions。
   - 剩余风险：四项旧 assertion 的最终迁移仍由 `task-test-2` closure gate关闭；未观察到新的 ordinary result failure。
   - 是否可 merge：是，本项不阻断。

3. **Finding 3 已闭合；本项可 merge。**
   - 证据：`src/async_sf_cc.erl:283-305` 的 state dispatcher在 take 后对 structural executor failure返回 `{error, Error, ConsumedState}`；`src/async_sf_cc.erl:718-752` 恢复 error传播与 active-fail `status => failed, error => Error`。
   - 闭合判断：preflight failure保持原 State，take 后 invalid return保留 consumed state且不 rollback/retry，`failed` 已可达；callback-internal runtime exception没有被该 designed-error branch吞掉。
   - 剩余风险：deferred suites 尚未迁移到新 status/summary shape，但这是 test-owned transition。
   - 是否可 merge：是，本项不阻断。

4. **Finding 4 已闭合；本项可 merge。**
   - 证据：`include/async_internal.hrl:2-13` 恢复 exact `#callback{cc,acc_ref,parent_ref}` 与 `#sf_callback{frame,acc_ref,parent_ref,timer_ref=undefined}`；`src/async_util.erl:84-126` 严格区分 ordinary/stable/intermediate/outer/accumulator/other；`decode_callback/1` 只接受 classifier 已确认的 action。`src/async_sf_cc.erl:95-106` 因而拒绝 intermediate frame node进入 external action route。
   - 闭合判断：record、classifier 与 router现在使用单一 representation authority，internal node不再可作为 reply target。
   - 剩余风险：旧 tuple/summary assertions仍由 deferred test migration移除，不是 production compatibility obligation。
   - 是否可 merge：是，本项不阻断。

5. **Finding 5 未完全闭合；阻断 merge。**
   - Immutable evidence：`src/async_sf_cc.erl:332-340` 的 `preflight_action_in_store/3` 只执行 action shape/resource检查及 ancestor preflight，没有查询 action 自身的 `?CHILDREN(Key)`。虽然 `preflight_children_direct/3` 在 `src/async_sf_cc.erl:400-424` 能拒绝 action children，但它只从 ancestor/node-owned traversal调用，不覆盖当前被 consume 的 action。随后 `take_action_value/3`（`src/async_sf_cc.erl:545-549`）只删除 action、timer sidecar与parent edge；若 store 含 action-owned reverse-index，该 index及child会在 consume 后残留。
   - Additional ownership evidence：`take_action_value/3` 已通过 `cleanup_direct_resources/3`（`src/async_sf_cc.erl:495-501`）demonitor reference，但 `src/async_t.erl:1076-1105` 在 `route_action/3` 成功后再次执行 `demonitor_ref/2`。cleanup 因而仍有两个 owner，与 accepted `async_sf_cc` single-owner及线性 preflight→take→cleanup 要求不符。
   - Additional timer risk：`register_timer/3`（`src/async_sf_cc.erl:40-59`）一律以 ordinary sidecar `timer_key(Key)` 判断重复；stable timer实际写在 `#sf_callback.timer_ref`（`:553-556`），重复调用会启动第二个 timer并覆盖第一个 reference。正常 `promise/3` 当前只有一次调用，但 helper自身未守住 direct resource invariant。
   - Observed vs required：observed 是 malformed action ownership metadata可绕过 preflight并在 take 后泄漏，monitor cleanup重复，stable duplicate timer可失去旧 timer owner；required 是 action及其全部 direct cleanup targets在任何 side effect前完成单一 preflight plan，take 后只由 `async_sf_cc` 清理一次，任何 duplicate/orphan registration不遗留资源。
   - Bounded correction route：Correction owner 为 `task-coding-2`，只需修改 `src/async_sf_cc.erl` 与 `src/async_t.erl`。让 `preflight_action_in_store/3` 明确验证 action不存在任何 reverse-index/children；让 timer duplicate检查按 ordinary sidecar/stable record各自的唯一 representation执行；删除 `async_t:handle_reference/3` 的重复 demonitor路径，使 `async_sf_cc` 成为唯一 cleanup owner。保持 message non-consumption、reply take-before-invoke、AccRef post-result cleanup与 map/list/dict backend不变。
   - Focused closure：证明 malformed action index在 consume前返回 error且 state/resources bit-for-bit不变；ordinary/stable duplicate timer不创建或遗失 timer；reply/timeout/DOWN每个 monitor/timer只清理一次；随后重跑 supplied blocking matrix与 `async_t_SUITE`，不得出现新 signature。
   - 剩余风险：在上述 correction 前，结构损坏或重复 timer seam可导致 orphan metadata/resource；当前不可 merge。
   - 是否可 merge：否。

6. **Finding 6 已闭合；本项可 merge。**
   - 证据：bounded search 在 affected paths已无 callback-body `try/catch`、`badfun` catch或 stack-first-frame `undef`推断。`src/async_util.erl:135-152` 对 external fun先 `ensure_loaded`/`function_exported`，只把 `nofile`/missing export规范化，其他 load error保留原 Reason；`src/async_sf_runtime.erl:49-53,85-94` 在 apply前确认 current MFA并原样返回其他 loader reason。
   - 闭合判断：availability 在 invocation 前直接查询；进入 callback/frame body 后的 `badfun`、`undef` 或 business exception按 native identity传播。
   - 剩余风险：purged local fun没有可证明的 preflight availability时仍原生抛出 `badfun`，这是避免误分类 callback-body异常的保守边界，不构成 unsupported translation。
   - 是否可 merge：是，本项不阻断。

7. **Finding 7 已闭合；本项可 merge。**
   - 证据：`src/async_sf_transform.erl:1003-1011` 的 manifest frame只含 `identity`、`mfa`、sorted `required_captures`；`src/async_sf_runtime.erl:55-71` 强制 `identity = undefined` iff unnamed，并只接受 named `{1,Block,StepName}`。
   - 闭合判断：manifest没有重复 `step_name` identity carrier，named/unnamed边界与 current-only lookup一致。
   - 剩余风险：旧 manifest tests的 path/async-id迁移仍属 `task-test-2`。
   - 是否可 merge：是，本项不阻断。

8. **Finding 8 已闭合；本项可 merge。**
   - 证据：`src/async_r_t.erl:49-76,333-365` 已删除 ambient continuation-context reader/writer及第三层 ReaderT，`run_scope/5` 只保留 AccRef；`run_sf_action/3,4`、runtime validation exports、`normalize_async_effect/1` 与旧 header compatibility注释均已删除，bounded caller search无上述 symbol。
   - Retained-clause disposition：`src/async_sf_transform.erl:126-127` 的 `format_error({duplicate_block_path,...})` 仍存在，但 compact context明确把 `async_sf_transform_tests` 的 exact formatter clauses定义为 current formatter contract，且 blocking formatter EUnit直接消费该 clause并通过。因此它具有本轮所需的 current consumer/contract evidence，不再属于“无依据 residue”；其 future retirement必须与 test-owned diagnostic contract同步，不能在本 production-only correction中单边删除。
   - 剩余风险：后续若正式退役该 diagnostic vocabulary，应由拥有测试迁移权限的任务同步更新；当前不影响 explicit Context、lifecycle或 transform identity。
   - 是否可 merge：是，本项不阻断。

# Verdict:

changes_required

# Changes:

- Correction owner：`task-coding-2`。
- 只实施 Finding 5 的 bounded production delta：在 `src/async_sf_cc.erl` 补齐 current action reverse-index/timer preflight并保证唯一 resource owner，在 `src/async_t.erl` 删除重复 demonitor路径。不要修改 tests、OpenSpec、plan、status 或 lanes，也不要重开 Findings 1–4、6–8 已接受的 correction。
- 新 immutable checkpoint 必须提供 `cb03fb4..new` 的 correction diff、Finding 5 focused closure、完整 blocking self-validation与 `async_t_SUITE` 精确结果。只有无新 product signature且 Finding 5 三个 resource/ownership缺口均关闭后才可 merge。

# Caveats:

- 本轮没有重新运行 supplied validation；命令结论采用用户提供的 independent evidence，并以 immutable correction diff、current affected workflows和 caller/omission search作语义核验。
- `async_t_SUITE` 四项 AccRef legacy assertions仍由既有 deferred owner `task-test-2` 关闭；本 review没有把它们重新路由为 coding failure，也没有扩展 accepted signature。
- macro及其他 deferred suites没有本轮新结果；其完整 closure gate仍由 `task-test-2` 在 coding candidate merged into integrate 后执行。
- 本轮未修改 source、tests、OpenSpec、plan、status 或 lanes，未 stage、commit、merge或delegate；唯一写入是本 review artifact。
