# Status:

changes_required

# Task:

task-coding-2

# Reviewed commit:

`aa5b0436514c044ae84bc00598153d45b26f4659`

# Previous reviewed commit:

`aa5b0436514c044ae84bc00598153d45b26f4659`（同一 immutable checkpoint 的 `task-coding-2-evidence-boundary-1`；本轮首次执行 semantic coding review）

# Evidence:

- 比较基线为 accepted base `0adf22a22f20c45fff192d52398f23f83bf4548c`。已确认 base 是 reviewed commit 的祖先；coding worktree 的 HEAD 精确为 reviewed commit 且 `git status --short` 为空，因此未把移动或未提交状态纳入证据。
- cumulative diff 只修改八个 task-owned production paths：`include/async_internal.hrl`、`src/async_r_t.erl`、`src/async_sf_cc.erl`、`src/async_sf_runtime.erl`、`src/async_sf_transform.erl`、`src/async_t.erl`、`src/async_util.erl`、`src/monad_frame.erl`，合计 750 insertions / 768 deletions；没有 test、OpenSpec、status、plan、lane 或非 owned production path 变更。
- candidate 已建立大部分目标骨架：所有 ordinary/sf public runner 经 `async_t:run_with_descriptor/3` 注册 outer node；action 使用 direct `parent_ref`；`async_sf_cc` 集中拥有注册、reverse index、take/cleanup、dispatch、inspection/fail；runtime 只解析 current frame；transform 保留 Astranaut collect/rewrite 两遍并生成 direct block/step MFA。bounded search 中 active production path 已无 `sf_cc_ref`、`sf_next`、`sf_cc_mref`、`block_path`、`async_id`、`crypto:hash`、`new_ref` 或 `terminal` caller。
- supplied independent validation：compile 0、full EUnit 33/0、macro CT 0、xref 0、dialyzer 0、diff check 0。accepted blocking 命令因此有通过证据；这些结果不替代下述语义审查。
- evidence-boundary round 已通过，且本轮不建议再次移动 validation command。linked suites 在 base/candidate 均不存在，仍由 `task-test-2` author；旧 shape assertion 的迁移也仍由 `task-test-2` 关闭。
- 但旧 suites 中并非所有观察都只是 stale shape：`async_sf_cc_SUITE:message_reply_timeout_down/1` 的公开断言是在 message 后 callbacks bit-for-bit 不变，candidate 实际把 action、outer node 与 reverse index 全部清空；`async_t_SUITE` 的 ordinary message/pmap cases 在 base 为 27/27，通过 candidate 后出现公开结果 `unhandled`、`badmap undefined` 与 `monad_badmatch undefined`。这些是 contract 明确要求保留的 ordinary/message/AccRef 行为，不能作为 test-owned legacy transition 接受。
- final cumulative omission pass 独立覆盖 runner → action entry → route → node dispatcher → current executor、frame resolution、transform collect/freeze/rewrite/manifest、inspection/failure：summary 未发现 Env/State/timer/monitor/accumulator/callback/Context 泄漏，active old recursive/root representation 已退役；但发现 manifest 冗余字段、不可达 error/status、未消费的 ambient context channel、零 caller export/legacy formatter clause、unsafe catches 与 cleanup-target preflight 缺口，详见 Findings。

# Findings:

1. **阻断：ordinary message 会消费 continuation node，并使后续 reply 无法 exactly-once 完成。**
   - Immutable evidence：`src/async_t.erl:1078-1123` 的 `handle_reference/3`、`execute_callback_a/5` 在 message 时保留 ActionMRef，却直接执行 stored ordinary `Callback({message, A})`；当该 callback 是 outer runner 时会进入 `src/async_sf_cc.erl:144-165` 的 consuming `dispatch/3`，take outer 后由 `cleanup_owned/3` 清掉其 action child 和 reverse index。candidate CT 的 `message_reply_timeout_down` 观察到 message 后 callbacks 从完整 pending store 变为 `{}`。
   - Contract basis：task contract capability disposition 的 `message non-consumption`、invariant 6，以及 compact context 对 message 不消费 ActionMRef 或 continuation node 的冻结要求。
   - Observed vs required：observed 是 message 消费 outer/action ownership graph；required 是执行既有 message continuation semantics，同时 action、current continuation node、timer及其 direct ownership edge仍 pending，随后 reply/timeout/DOWN 只完成一次。
   - Correction owner：`task-coding-2`。Allowed paths：`src/async_t.erl`、`src/async_sf_cc.erl`，必要时 `src/async_sf_runtime.erl`。建立明确的 non-consuming message terminal/route，不得复用 consuming outer dispatch；保持 reply 路径的 take-before-invoke 与 late/duplicate unhandled。
   - Focused closure：ordinary 与 stable 的 message→reply、message→timeout、message→DOWN 均证明 message 前后 action/node/reverse index不变，最终事件后全部 direct resources 清理且 callback count 为一。

2. **阻断：reply 在运行 current continuation 之前删除最后一个 AccRef，破坏 ordinary/parallel compatibility。**
   - Immutable evidence：`src/async_sf_cc.erl:44-53` 的 `decode_action/3` 在返回 decoded action 前调用 `remove_orphan_accumulator/2`；`src/async_t.erl:950-963` 随后才用 decoded `acc_ref` 调 `async_r_t:run_scope/6`。当被消费的是最后一个引用该 AccRef 的 action 时，continuation 进入 scope 前对应值已经不存在。
   - Contract basis：bounded goal 要求 ordinary behavior 与 action resource lifecycle 不变；invariant 2/6 要求 AccRef 与 ContinuationRef 独立并在 reply/timeout/DOWN 恢复两个 scope；context 冻结 ordinary parallel/accumulator regression。
   - Observed vs required：candidate 的 ordinary `pmap`/custom cc/with-acc 路径读到 `undefined`，产生 `badmap undefined`、`monad_badmatch undefined` 或 `unhandled`；required 是 current callback 在原 AccRef scope 中运行，只有其 continuation 完成或明确取消且没有新 owner 后才清理 accumulator。
   - Correction owner：`task-coding-2`。Allowed paths：`src/async_sf_cc.erl`、`src/async_t.erl`、`src/async_r_t.erl`。action take 时先保留可执行 scope，callback/effect 结束后再按新 pending owners决定删除；parent cancellation 可以直接清理，因为不会 invoke current continuation。不得把 accumulator 放入 frame/identity/summary。
   - Focused closure：重跑 ordinary message、`progn_par`、map/list/dict pmap、custom async cc、multiple messages 与 local accumulator cases，公开结果与 base 一致；direct `/3` outer callback仍只看到最终 CleanState。

3. **阻断：`dispatch_continuation/4` 未实现 accepted consumed-state error branch，active failure 的 `failed` 状态不可达。**
   - Immutable evidence：`src/async_sf_cc.erl:167-183` 的 `dispatch_state/5` 只有 `{ok,...}` 与 `{unhandled,...}` 返回；`dispatch_mode/4` 在 preflight 或 take 后执行失败时以异常逸出。`fail_stable/5`（397-417）只处理 `forwarded/unhandled/handled`。最终 commit `aa5b043` 还从上一 checkpoint 删除了 `{error, Error, State2} -> status => failed` 分支以消除 dialyzer warning，证明该结果在当前实现中确实不可产生。
   - Contract basis：accepted normative excerpt 精确规定 `dispatch_continuation` 返回 `{ok, DispatchValue, NewState} | {unhandled, State} | {error, Error, ConsumedState}`；compact context 精确规定 `forwarded | handled | failed | unhandled | ordinary` statuses；take 后不得 rollback/retry。
   - Observed vs required：observed 是 take 后 invalid return/structural execution failure丢失可返回的 ConsumedState并以异常越过 state API，`failed` 永远缺失；required 是 state adapter保留已消费状态和错误身份，active fail据此返回 `failed`，同时 monadic/public callback-internal exception仍按其 native contract传播。
   - Correction owner：`task-coding-2`。Allowed path：`src/async_sf_cc.erl`。把 state-level dispatch 分成 pure preflight/take/clean 与 executor 两段，确保 take 后可形成 `{error, Error, ConsumedState}`，恢复 `fail_stable` 的 `failed` 处理；不得恢复 node、retry 或 root-search。
   - Focused closure：覆盖 preflight failure state unchanged、take 后 invalid return/error state consumed、duplicate unhandled、active-fail `failed` 携带 exact error，以及 earlier selected node不回滚。

4. **阻断：action record 与 classifier/router 不符合 accepted exact representation，internal frame node 可被 external reply 路由。**
   - Immutable evidence：`include/async_internal.hrl:3-15` 给 ordinary `#callback{}` 增加了 contract 未包含的 `timer_ref`，并给两个 record 的 `parent_ref` 加默认值；accepted exact shape 是 `#callback{cc,acc_ref,parent_ref}` 与 `#sf_callback{frame,acc_ref,parent_ref,timer_ref=undefined}`。同时 `src/async_sf_cc.erl:59-68` 的 `action_preflight/3` 接受任意 `#sf_callback{}`，包括 `acc_ref = undefined` 的 intermediate frame node；`src/async_util.erl:84-102` 的 decoder也不阻止该跨分类。任意 map/list/tuple 一律标成 accumulator，scalar accumulator却标成 other，也不是一致 classifier authority。
   - Contract basis：normative record excerpt、invariant 3/4、backend/classifier disposition：ordinary/stable/intermediate/outer/accumulator/other 必须一致区分，external router 只认 action，internal node 不是 reply target。
   - Observed vs required：observed 是额外 ordinary representation、默认化缺失 parent、以及 intermediate `#sf_callback{}` 可从 `route_action/3` 解码；required 是 one exact record encoding、显式 direct parent、stable action 以 reference AccRef 与 intermediate node隔离。
   - Correction owner：`task-coding-2`。Allowed paths：`include/async_internal.hrl`、`src/async_util.erl`、`src/async_sf_cc.erl`、`src/async_t.erl`、`src/async_r_t.erl`。恢复 exact records；ordinary timer使用 `async_sf_cc` 私有 non-reply resource ownership而不扩展 record或包裹 actual K；router先按严格 action classifier检查有效 AccRef，internal Ref 必须返回 unhandled。
   - Focused closure：验证 exact tuple shape、map/list/dict backend matrix、stable/ordinary action reply、intermediate/outer Ref external reply unhandled、`callback_exists/1`只统计 pending actions。

5. **阻断：cleanup target 未在 take 前完整 preflight，部分 action resource side effect 又发生在 take 前。**
   - Immutable evidence：`src/async_sf_cc.erl:185-227` 的 `preflight_node_in_store/3` 只检查 current node与parent chain，不检查 `?CHILDREN(Ref)` reverse-index shape、child existence/kind、timer/monitor/accumulator cleanup targets；`children/2`（460-465）对 malformed index静默返回 `[]`，于是 parent 可被消费而 children 泄漏。`decode_action/3`（44-53）及 `take_action/4`（419-432）先 cancel timer/demonitor，再从 callbacks take action。`src/async_t.erl:431-441` 又在 `register_action/3` 成功前启动 timer，duplicate/orphan registration error会留下 timer。
   - Contract basis：exact order 为 preflight current node shape/direct cleanup targets → take → cancel/demonitor/remove direct resources → CleanState → invoke/forward；resource leak和 mutation-before-preflight 不可接受。
   - Observed vs required：observed 的 malformed ownership metadata被当作无 children，且 timer/monitor side effect可能先于 take或在注册失败后泄漏；required 是先冻结并验证完整 direct cleanup plan，失败时 state/resources不变，成功时 take 后线性清理。
   - Correction owner：`task-coding-2`。Allowed paths：`src/async_sf_cc.erl`、`src/async_t.erl`。增加 cleanup-plan preflight，验证 index和每个 direct owned entry/resource；先构造 taken callbacks，再执行 cancel/demonitor；timer只在 registration commit 可保证时创建，或在 registration failure精确撤销。
   - Focused closure：malformed index/child/resource preflight不 mutation，正常 take 的 node/action/timer/monitor/accumulator 顺序正确，registration duplicate/orphan不留下 timer，late/duplicate事件 unhandled。

6. **阻断：availability catches 会吞掉 callback/frame 内部同形异常，非-`nofile` load error也被改写。**
   - Immutable evidence：`src/async_t.erl:1115-1123`、`src/async_sf_cc.erl:274-283,344-346` 捕获 `error:{badfun, Callback}`，但 catch覆盖整个 user callback body；用户代码主动抛出同一 reason会被误报为 unavailable并向 parent forward。`src/async_sf_runtime.erl:46-61` 仅凭 `undef` stack首帧等于 current MFA 判 unavailable，current frame body中的 `erlang:error(undef)`同样满足该判定。`src/async_util.erl:120-129` 与 `src/async_sf_runtime.erl:100-109` 把其他 loader error包装成 `module_load_failed`，没有原样返回/传播。
   - Contract basis：invariant 7与 simplicity exception standard：只在 nearest call boundary term-exact转换可证明的 current availability；进入 user callback后的异常保留原 class/reason/stack；只有 `nofile`/missing exported MFA可规范化，其他 load error原样传播。
   - Observed vs required：observed 是根据可由用户代码伪造的 reason/首帧猜测 capability absence并改写 loader error；required 是 preflight module/export availability，并只转换能证明发生在 invocation boundary 的 absence，nested/internal异常 native re-raise。
   - Correction owner：`task-coding-2`。Allowed paths：`src/async_t.erl`、`src/async_sf_cc.erl`、`src/async_sf_runtime.erl`、`src/async_util.erl`。统一一个 exact availability seam；移除跨 callback body 的 broad catches，或用不可由 body混淆的 call-site证据窄匹配；保持原 stacktrace和非-`nofile` loader outcome。
   - Focused closure：missing module/export规范化；callback body主动 `badfun`、主动/nested `undef`、business crash原样传播；其他 loader error不换 identity；take 后仍不 rollback。

7. **阻断：generated manifest 泄漏重复的 `step_name`，runtime 允许 named frame 缺失 direct identity。**
   - Immutable evidence：`src/async_sf_transform.erl:1003-1012` 的 manifest entry 同时输出 `identity` 与独立 `step_name`；accepted manifest public fields不包含该重复字段。`src/async_sf_runtime.erl:83-86` 的 `valid_identity/3` 接受 `identity = undefined` 且 `step_name` 为 atom，因此形如 named frame但没有 `{1,Block,StepName}` identity 的数据可通过 shape validation并尝试 same-version resolution。
   - Contract basis：named identity精确 `{Protocol,Block,StepName}`；manifest只公开 current owner、literal Vsn/`undefined`、identity、MFA、sorted required captures；unnamed locator不得成为 migration identity。
   - Observed vs required：observed 是 manifest有无消费者的重复 identity分量，且 named/unnamed representation边界可模糊；required 是 named frame始终携带 exact tuple，只有真正 unnamed boundary使用 `undefined`，manifest entry无额外 identity副本。
   - Correction owner：`task-coding-2`。Allowed paths：`src/async_sf_transform.erl`、`src/async_sf_runtime.erl`、`src/monad_frame.erl`。删除 manifest entry 的 `step_name`；把 runtime invariant收紧为 `identity = undefined` iff `step_name = undefined`，named lookup只按 exact identity；frame/inspection仍可保留 documented `step_name`。
   - Focused closure：manifest key set精确、captures排序；malformed named-without-identity frame在 invoke前失败并只forward direct parent；named move/insert稳定、rename变化、unnamed仅same-version resume。

8. **重要 simplicity finding：Continuation context 同时存在 explicit 与无消费者 ambient channel，并保留零 caller接口/立即二次 normalize。**
   - Immutable evidence：实际语义通过 `async_t` 的 `(K,SfContext)` 显式闭包传播；但 `src/async_r_t.erl:60-79,220-247` 仍导出/生成 `get_continuation_context/1` 与 `local_continuation_context/3` reader channel，repository 内前者零 caller，后者写入的数据无 reader。`inner_async_r_t/3`（13-22）甚至仍把该 slot声明为旧 `reference() | undefined`，与写入 map context不一致。`src/async_sf_cc.erl:297-349` 的 `normalize_async_effect/1` 只把同一 `async_r_t` unwrap后立即 rewrap；最终 commit专为 dialyzer加入该无语义跳转。bounded caller search还发现 `async_t:run_sf_action/3,4`、`async_sf_runtime:validate_frame/2`、exported `validate_frame_shape/1` 无 current caller，`format_error({duplicate_block_path,...})` 无 producer，header 的“三元 tuple compatibility boundary”注释与实际 decoder/当前 contract均不符。
   - Contract basis：task-specific simplicity要求 Context/ContinuationRef各有单一显式 channel、无立即二次 normalize private shape、无没有 current caller/contract 的 legacy adapter/export，并要求 consumer-driven interfaces 与 internally consistent representation。
   - Observed vs required：observed 是 duplicate/dead context carrier、旧 slot type、无语义 wrapper及未证明的 compatibility residue；required 是 explicit SfContext唯一 owner/channel，每个 export或兼容分支有当前消费者与合同依据。
   - Correction owner：`task-coding-2`。Allowed paths：`src/async_r_t.erl`、`src/async_t.erl`、`src/async_sf_cc.erl`、`src/async_sf_runtime.erl`、`src/async_sf_transform.erl`、`include/async_internal.hrl`。移除无 reader 的 ambient continuation channel并同步 transformer type/runner；直接传递 accepted `Effect`；对零 caller export/legacy clause逐项删除，除非能在 bounded current contract/caller中给出保留证据。
   - Focused closure：compile/xref/dialyzer通过；public runner→outer→action→dispatcher trace不增加隐藏 context channel；export/caller inventory无零消费者残留，主路径无 unwrap/rewrap跳转。

# Verdict:

changes_required

# Changes:

- Correction owner：`task-coding-2`。
- 只在上述 task-owned production paths 内完成八项 bounded correction；不修改 tests、OpenSpec、status、plan、lanes，也不重开或移动 evidence-boundary contract delta。
- correction checkpoint 必须先运行每项 focused closure，再运行 accepted blocking self-validation；同时重跑 deferred suites用于确认 ordinary/message/AccRef 产品失败已消失，剩余失败若有只能是 evidence-boundary 已接受的精确 stale shape assertions。任何新签名继续阻断。
- correction review 应提供从 base 到新 immutable checkpoint 的 cumulative diff，并重新执行 correctness、simplicity 与独立 final omission pass；必须恢复 `dispatch_continuation` error/`failed`、ordinary message/parallel behavior、exact records/classifier、cleanup ordering、native exception边界及 exact manifest。

# Caveats:

- 本轮未编辑或执行任何 source/test/status/plan/lanes/OpenSpec 文件，也未 stage、commit、merge或delegate；唯一写入是本 review artifact。
- 本轮没有重复运行 supplied blocking matrix；validation结论采用用户提供的 independent results，并以 immutable source/diff及现有 candidate CT记录做语义定位。compile/EUnit/xref/dialyzer通过不抵消公开行为与合同缺陷。
- linked suites 在 base/candidate 均缺失仍是 `task-test-2` 的 authoring责任；本 review没有把该缺失误路由给 coding owner。相反，ordinary message/pmap/AccRef failures已有现行合同与公开旧回归 oracle，属于 coding product defect，不能由 test migration掩盖。
- transform 的 collect/freeze/rewrite总体保持单 pipeline，summary privacy也基本满足；本 verdict不要求重写这些已正确部分，只要求修复 Findings 指定的 bounded缺口并在 correction round重新做 cumulative omission pass。
