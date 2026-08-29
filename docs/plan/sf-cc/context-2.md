# Initiative Context

## Binding

- Goal: sf-cc
- Generation: 2
- Predecessor context: `none`
- Predecessor disposition: `unrelated`
- Collected at: 2026-08-28 Asia/Shanghai
- Evidence target commit/tree: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Target before collection: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Target after collection: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Currency: `current`
- Repository revisions: 仅使用 control repository `/home/slepher/project/erl_projects/async-sf-cc`；current-code/test evidence 均来自上述 revision，history evidence 仅来自该 revision 可达的 bounded path history。
- Normative inputs: `openspec/changes/sf-cc/**`、`openspec/changes/sf-do/**`；`tasks.md` 仅作为非规范执行线索；未读取 `sf-migrate`。
- Authorized history ranges: `0adf22a22f20c45fff192d52398f23f83bf4548c` 可达的 `6dac66f7da63dda3ef3c2e00377c8cfe3023649c^..0adf22a22f20c45fff192d52398f23f83bf4548c`，且仅限触及 sf-cc/sf-do OpenSpec inputs 或本 packet 指定的 sf-cc 相关实现/测试路径；未访问其他 repository。
- Collection contract: 记录 sf-cc 当前规范要求、目标 revision 的实现 capability/interface/caller/test 与 bounded history，逐条绑定搜索字符串、命令、scope、精确 location、revision、finding 和 limits；区分规范、当前事实、历史、inference、conflict、stale/unknown；不作架构或 reuse/extend/new/replace 决策。

## Generation independence

- Previous bindings: none; generations are independent
- Generation delta: not derived from a predecessor; derive only from current inputs and target
- Retained and rebound evidence IDs: none
- Invalidated evidence IDs: none
- Prior gaps addressed: none; predecessor gaps do not block this generation
- Unchanged searches not repeated: none; do not reuse predecessor searches

## Task-relevant normative requirement index

- `sf-cc` 要求所有 public runners（`wait/1`、`wait_t/2`、ordinary runners、`run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5`）进入同一 `ContinuationRef` lifecycle；sf-prefixed macro 只接受显式 external fun，保留 fun term 和 opaque Context，并区分 async `/2`、direct-state `/2` 与 `/3` return contract（`openspec/changes/sf-cc/specs/sf-cc/spec.md:5-165`；`design.md:68-156`）。
- `sf-cc` 要求 ordinary action 使用 `#callback{cc,acc_ref,parent_ref,...}`，stable action 使用 `#sf_callback{frame,acc_ref,parent_ref,timer_ref}`；所有 continuation node 使用同一种 `ContinuationRef`，只有 direct `parent_ref`，不复制 root/final Ref，且 reader/classifier 必须一致（`spec.md:167-250,372-384`；`implementation.md:31-85,107-124,191-205`）。
- `sf-cc` 要求 dispatch 在调用前完成 preflight、take、direct resource cleanup，再按 node kind 执行或只向 direct `parent_ref` 转发；每个 node exactly-once、不可 rollback/retry，ordinary business Outcome 仍先进入当前 `cc`，只有可精确归属 current continuation availability 的 `badfun`/`undef` 才外移一层（`spec.md:266-350`；`implementation.md:126-189`）。
- `sf-cc` inspection/failure 必须 current-node sanitized summary、matcher preflight atomicity、selected Ref order 和 `forwarded | handled | failed | unhandled | ordinary` statuses；不得公开 Env/State/timer/monitor/accumulator/callback/Context（`spec.md:401-465`；`implementation.md:207-249`）。
- `sf-do` 必须保持原 Monad 类型，使用 `monad_frame:bind_frame/4` 与 `then_frame/4`，generated frame `(Input, Env)` 直接返回 remainder Monad；local `return/1`、`fail/1` 按 lexical Monad rewrite，final action 不生成 frame（`openspec/changes/sf-do/specs/sf-do/spec.md:5-16,77-107,150-210`；`implementation.md:12-78,144-191`）。
- `sf-do` 必须显式传播 `(K,SfContext)`，bind_frame 自动物化 direct parent；nested stable representation 是 `InnerFrame + OuterRef`，owner/module/Vsn resolution、minimal Env、pattern semantics、manifest 与 named identity 均不依赖 root chain（`sf-do/spec.md:109-148,229-350,360-477`；`design.md:178-234,260-364,366-467`）。
- named identity 必须精确为 `{Protocol, Block, StepName}`，generated MFA 固定为 `'$sf_<block>__<step>'/2`；不得使用 `AsyncId`、hash、Action AST、source index、lexical parent 或 capture layout；unnamed boundary 只可普通执行/same-version resume，migration 前 fail closed（`sf-do/spec.md:360-468`；`implementation.md:304-322`）。
- `sf-do` compiler 必须用 AST-aware 两遍 traversal：Pass 1 冻结 occurrence/digest/plan 并累计 diagnostics，有 error 时不得进入 Pass 2；Pass 2 只消费 frozen plan；manifest 只公开 current owner/Vsn/identity/MFA/required captures（`sf-do/spec.md:486-567`；`design.md:469-598`）。

## Brief project orientation

本 packet 只覆盖 sf-cc/sf-do 合同直接经过的 callbacks store、async runner、frame bind/runtime、transform、macro 与指定测试。当前代码的主路径是：`async_m` facade → `async_t` runner/router → `async_r_t` reader/state/store → `async_sf_cc` completion facade 或 `async_sf_runtime` frame continuation；`async_sf_transform` 负责 `sf_do` source lowering，`monad_frame` 暴露 frame capability。

## Task-relevant capability map

以下所有 current-code 事实均绑定 revision `0adf22a22f20c45fff192d52398f23f83bf4548c`；每条均给出精确文件与行范围，汇总证据见 E-4 至 E-13、E-19。

- 宏门禁存在：`include/async.hrl:1-5` 注册 `async_sf_cc_macro` 的三个 sf-prefixed macros；目标 revision 的 `src/async_sf_cc_macro.erl:11-51` 识别 `{'fun', Pos, {function, ModuleExpression, FunctionExpression, {integer, _, Arity}}}`，检查允许 arity，并生成普通 `async_m` remote call。它未验证 runtime Promise/Context/State。
- public facade 存在：目标 revision 的 `src/async_m.erl:34-40,202-233` 导出并转发 `run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5`、`inspect_sf_callbacks/3`、`fail_sf_callbacks/4`；`run_sf_frame/4` 仍作为旧 descriptor/MFA 入口存在于 `:206-212`。
- async context 已显式进入 inner action：`src/async_t.erl:13-23,232-249` 的 `inner_async_t` 接收 `(K, SfContext)`，`'>>='/3` 通过 `contextual_real_to_async_t/2` 传递它，`bind_frame/4` 调用 `async_sf_runtime:new_frame/2` 后进入 `run_frame_action/5`。当前 context shape 是 `#{continuation := term(), runner := fun((term()) -> term())}`，不是规范要求的 `current`/`parent_ref` shape。
- frame capability facade 存在：`src/monad_frame.erl:21-56` 导出/声明 `bind_frame/4`、`then_frame/4` 和 `default_then_frame/4`；`src/async_t.erl:243-252` 实现 `bind_frame/4`，`then_frame/4` 直接走 `default_then_frame/4`。当前 frame context 类型仍含 `block_path`、`async_id`、`env`。
- ordinary/stable action writer 路径存在但仍旧编码：`src/async_t.erl:424-467` 的 `promise/3` 读取 `get_local_ref/1` 和 `get_sf_cc_ref/1`，用 `callback_continuation/2` 计算 continuation，随后存储 `#callback{cc = NK, acc_ref = AccRef, sf_cc_mref = CcRef}`；timer resource 以 `{sf_cc_timer, Ref, MRef}` 存储。
- runner construction 仍是 CcRef model：`src/async_t.erl:788-820` 的 `run_with_cc/5`、`run_cc/3`、`run_sf_cc/4`、`run_with_sf_cc/6`、`exec_sf_cc/6` 都经 `run_with_descriptor/3`；该函数 `make_ref/0`、写入 `async_sf_cc:new_ref/2`，再以 `async_sf_cc:terminal/1` 作为 stable continuation。
- nested frame path 仍是 recursive map：`src/async_t.erl:822-855` 的 `run_frame_action/5` 用 `async_sf_runtime:new_next/2`；stable runner context 下的 ordinary downstream 另建 CcRef descriptor，随后仍把 `new_next(Frame, async_sf_cc:terminal(CcRef))` 交给 `run_sf_action/3`。
- reply/message/timeout/DOWN routing 由 `src/async_t.erl:891-975,1089-1160` 负责：message 保留 callback entry，reply 删除 ActionMRef 并清理 timer resource，`execute_callback_a/5` 对 function 直接调用、对 `sf_next`/`sf_cc_ref` 调 `async_sf_runtime` runner；timeout/DOWN 由既有 action outcome 路径进入 callback。
- `async_r_t` 提供独立 AccRef reader 与 callbacks State store API：`src/async_r_t.erl:209-243,278-343,362-368` 的 `get_local_ref/1`、`get_sf_cc_ref/1`、`local_ref/3`、`local_sf_cc_ref/3`、`find_ref/2`、`put_ref/3`、`remove_ref/2`、`run_scope/6` 现有可调用边界仍使用第二个 CcRef reader local。
- `async_sf_cc` 当前拥有 descriptor validation、CcRef prepare/cleanup/invoke、inspection 和 active failure：`src/async_sf_cc.erl:21-120,122-241,243-326`。它以 `sf_cc_ref` map 作为 terminal，按 CcRef 扫描/清理 owned callback，调用 external final callback，并按 CcRef 去重 active failure。
- `async_sf_runtime` 当前拥有 recursive `sf_next` resolution：`src/async_sf_runtime.erl:16-121` 定义 `sf_frame`、`sf_cc_ref` 和 `sf_continuation`，`continue_valid/4` 递归处理 frame/downstream 或 terminal；`src/async_sf_runtime.erl:130-284` 负责 current manifest、`block_path`/`async_id` identity、MFA 与 Env 检查，`src/async_sf_runtime.erl:286-308` 将 structural failure 送入 `get_sf_cc_ref/1` 的 CcRef completion。
- `async_sf_transform` 已有 two-pass skeleton 与 frame lowering：`src/async_sf_transform.erl:13-102,162-230,338-405` 用 Astranaut collect/rewrite、occurrence plan、digest 和 post-traversal generated forms；`src/async_sf_transform.erl:425-700,804-910` 实现 source validation、marker pairing、minimal capture 计算、`bind_frame`/`then_frame` 和 generated `(Input, Env)` frame。
- transform 的当前 identity/manifest 仍是旧 derived model：`src/async_sf_transform.erl:702-736` 用 `crypto:hash/2`、hex 和 `$sf_` digest；`:985-1018` 生成含 `block_path`、`async_id` 的 context 与 `blocks[Path].asyncs[AsyncId]` manifest。

## Task-relevant reusable interface inventory

- `async_sf_cc_macro:run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5`：macro provider export 位于 `src/async_sf_cc_macro.erl:8-18`，visibility 为 `-export_macro`；输入 source AST 中的 callback 在 `:25-45` 被检查，accepted/rejected arity 是其现有 extension boundary；调用者是 `include/async.hrl:3-5`。当前 limits：只证明 source-form gate，不证明 new node lifecycle。
- `async_m:run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5`：public export 位于 `src/async_m.erl:34-37`，实现为 `async_t` delegation `:214-223`；`run_sf_cc/3` 选择 `async_r_t:new(identity)`，direct-state APIs 经过 `identity:run/1`。当前 owner 是 facade，caller 是 `async_sf_cc_macro` 生成的普通 remote call；lifecycle 仍由 `async_t:run_with_descriptor/3` 提供旧 CcRef。
- `monad_frame:bind_frame/4`、`then_frame/4`、`default_then_frame/4`：public export 和 callback contract 位于 `src/monad_frame.erl:8-56`；`async_t` 是当前 `monad_frame` instance adapter（`src/async_t.erl:30-56,243-252`），返回原 `async_t` 类型。当前 extension point 是 typeclass dispatcher；当前 lifecycle 由 `run_frame_action/5` 和 recursive `new_next/2` 接管。
- `async_t:promise/3` 与 `async_t:run_reply/5`：writer 位于 `src/async_t.erl:431-461`，router/decoder 位于 `:956-975,1089-1112`；代表 callers 是 `async_m:promise/2`、`async_m:map_promises/2` 及 `test/async_t_SUITE.erl:170-228,283-349,514-817`。当前 contract 保留 ActionMRef、timer、monitor、AccRef，但 callback record 无 `parent_ref`。
- `async_sf_cc:dispatch/3`、`complete/3`、`complete_state/4`、`inspect/3`、`fail/4`：export 位于 `src/async_sf_cc.erl:9-19`，caller 是 `async_sf_runtime:continue/4`、`async_m:inspect_sf_callbacks/3`、`async_m:fail_sf_callbacks/4`；当前 `prepare/4`（`:103-120`）是 CcRef take/cleanup boundary，`apply_available_callback/3`（`:216-232`）是 final callback availability boundary。当前限制：没有统一 `dispatch_continuation/4`，没有 node-shaped `ContinuationRef` reader。
- `async_sf_runtime:new_frame/2`、`new_next/2`、`continue/4`、`validate_continuation/1`：export 位于 `src/async_sf_runtime.erl:6-14`；generated frame caller 是 `async_t:bind_frame/4`，reply caller 是 `async_t:execute_callback_a/5`。当前 `new_next/2` 只接受 recursive `sf_continuation()`，`continue/4` 对 invalid frame 通过 terminal CcRef completion；当前 owner/resolution boundary 使用旧 `block_path`/`async_id` manifest。
- `async_util:decode_callback/1`、`callback_value_kind/1`、`callback_exists/1`、`fun_availability/1`：export 位于 `src/async_util.erl:11-16`，map/list/dict backend 位于 `:23-64,97-102`；当前 decoder `:82-95` 只识别 `#callback{cc,acc_ref,sf_cc_mref}` 和 `sf_cc_ref`，`callback_exists/1` 只统计 `action_callback`。当前 owner 是 shared store utility，limits 是 absence of `parent_ref`/`#sf_callback{}`/intermediate-vs-outer classifier。

## Task-relevant existing behavioral coverage

以下所有 test 事实均绑定 revision `0adf22a22f20c45fff192d52398f23f83bf4548c`；测试通过或断言的含义不扩展为最新规范完成证明。

- macro source forms：`test/async_sf_cc_macro_SUITE.erl:10-49` 覆盖 external `/2|3` acceptance、local/anonymous/variable/runtime expression 与错误 arity rejection、source position；它不覆盖 new linked node lifecycle。
- sf-do source/lowering semantics：`test/async_sf_SUITE.erl:56-132` 覆盖 source grammar、marker、duplicate、unnamed warning、多错误；`:134-167` 覆盖 bind/then/final/local return/fail/remote call；`:250-302` 覆盖 minimal Env、case/receive/try equality、fun shadow、pattern；这些测试对当前目标 revision 的 manifest/old runtime 仍可作为 observable baseline，但不是最新 linked contract 的完成证明。
- sf-do old stable representation：`test/async_sf_SUITE.erl:169-209` 明确断言 `sf_next`、`sf_frame`、`sf_cc_ref`；`:236-248` 明确断言 nested continuation kinds `[sf_next, sf_next, sf_cc_ref]`；`:304-340` 明确断言 SHA/hex derived function name 与 `blocks[path].asyncs[async_id]` entry。
- sf-do old runtime failure/lifecycle：`test/async_sf_SUITE.erl:211-234,342-389` 覆盖 message/reply/timeout/DOWN、structural runtime errors、unnamed appup/appdown rejection，但 structural failure 的 helper `:427-447` 仍沿 recursive downstream 找 terminal CcRef。
- sf-cc runner/store/cleanup：`test/async_sf_cc_SUITE.erl:14-107` 覆盖 runner modes、Context、AccRef/CcRef distinction、pending store；`:109-189` 覆盖 cleanup-before-final callback、exactly-once、callback/effect exception boundaries；`:191-216` 覆盖 message/reply/timeout/DOWN；这些 assertions 仍以 old CcRef encoding 为前提。
- sf-cc inspection/failure：`test/async_sf_cc_SUITE.erl:218-335` 覆盖 old terminal structural completion、ordinary tuple callback、matcher privacy/order/preflight、active-fail CcRef dedupe；summary keys 在 `:289-298` 明确包含 `frames`、`sf_cc_mref`、`block_path`、`async_id`，与最新 current-only linked summary 冲突。
- ordinary async regression：`test/async_t_SUITE.erl:152-228,274-349,514-817,829-878` 覆盖 message、timeout、DOWN、sequential promise、parallel/map/list/dict-like callback store paths、accumulator 和 ordinary wait behavior；未发现针对 `parent_ref`、linked node take 或 reverse index 的测试。
- transform formatter unit coverage：`test/async_sf_transform_tests.erl:5-104` 覆盖 exact formatter clauses、unknown `function_clause`、Astranaut fallback、empty comprehension AST diagnostic；它证明当前 formatter contract，不证明 linked runtime contract。

## Historical evidence

- `6dac66f7da63dda3ef3c2e00377c8cfe3023649c`（2026-08-26，`feat: migrate sf-do implementation onto clean history`）在 bounded path 中引入/修改 `src/async_sf_transform.erl`、`src/async_sf_runtime.erl`、`src/async_t.erl`、`src/monad_frame.erl`、`src/async_m.erl` 与 sf-do tests；同一 commit 的文件清单显示这是当前 old recursive frame/CcRef baseline 的来源。它是 history evidence，不是当前规范或完成证明。
- `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03`（2026-08-26，`test(sf-cc): close deferred fixtures and coverage`）新增 `test/async_sf_cc_SUITE.erl`、`test/async_sf_cc_macro_SUITE.erl` 并修改 `test/async_sf_SUITE.erl`、`test/async_t_SUITE.erl`；因此当前可观察 old CcRef/recursive assertions 有明确历史来源。
- `7fc7db0896a24556bd83c0b59ae60be9e35d0ba0`（2026-08-26，`sf-cc-fix-cleanup-before-invoke`）只在 `src/async_sf_cc.erl` 修正 CcRef prepare 的 accumulator snapshot 与 cleanup-before-invoke；该历史说明旧 lifecycle 在 linked rewrite 前有过增量修正，不能外推为新 node semantics。
- `8bbefd3f8d6c45a2cf4da97951c501d53963df62`（2026-08-27，`Refine sf_do transform diagnostics`）修改 `src/async_sf_transform.erl`、`test/async_sf_SUITE.erl` 与 `test/async_sf_transform_tests.erl`，并增加 formatter/多诊断覆盖；这与当前 two-pass/source-diagnostic facts 相关，但不改变 old runtime representation。
- `aec8422cf1e3b8c28c2b792ae5195999ee69a661`（2026-08-28，`docs-freeze-linked-continuation-contract`）的 name-status 仅显示 sf-cc/sf-do OpenSpec 文档路径；`git diff --name-status aec8422... 0adf22a... --` 在本 packet 指定 code/test scope 无输出。该 bounded history 支持：最新 linked contract 文档变更没有对应 code/test 变更。
- `0adf22a22f20c45fff192d52398f23f83bf4548c`（2026-08-28，`docs: simplify sf continuation identity`）在 sf-cc/sf-do 文档中收敛 `parent_ref`、node classification、direct identity 与 named MFA；当前 changelog 将 linked contract 标为“未实现”。本 claim 绑定当前 revision 的 OpenSpec document，而非旧 generation artifact。

## Conflicts, stale evidence, inferences, and unknowns

### Conflicts

以下 implementation/test conflict claims 均绑定 revision `0adf22a22f20c45fff192d52398f23f83bf4548c`，并由 E-4、E-7、E-9、E-10、E-12、E-14、E-15 交叉支持。

- 最新 `sf-cc`/`sf-do` normative contract 要求 `#callback{cc,acc_ref,parent_ref,...}`、`#sf_callback{frame,acc_ref,parent_ref,timer_ref}`、single-shape `ContinuationRef` 与 `dispatch_continuation`；目标 revision 的 `include/async_internal.hrl:1-7` 仍只有 `#callback{cc,acc_ref,sf_cc_mref}`，且 bounded current-code search 没有 `parent_ref`、`#sf_callback` 或 `dispatch_continuation`。
- 最新 contract 要求 `sf_do` named identity 为 `{Protocol,Block,StepName}` 与 `'$sf_<block>__<step>'/2`；目标 revision 的 transform/runtime/test 仍使用 `block_path`、`async_id`、SHA/hex 和 recursive `sf_next`，具体见 `src/async_sf_transform.erl:702-736,985-1018`、`src/async_sf_runtime.erl:16-30,202-275` 与 `test/async_sf_SUITE.erl:304-340`。
- 最新 contract 要求 structural failure 只向 direct `parent_ref` 外移一层、ordinary Outcome 先调用当前 `cc`；目标 revision 的 `async_sf_runtime:complete_current/2`（`:299-308`）通过 CcRef terminal completion，`async_sf_cc:prepare/4`（`:103-149`）按 root CcRef 扫描/清理，且 tests 仍期待 terminal completion/CcRef dedupe。
- 最新 inspection contract 要求 current-only safe summary；目标 revision 的 `async_sf_cc:summary/2`、`continuation_summary/1`、`frame_summary/1`（`:243-277`）暴露 `sf_cc_mref`、`protected`、recursive `frames`、`block_path`、`async_id`，测试 `test/async_sf_cc_SUITE.erl:277-298` 固定该旧 shape。

### Stale evidence

- `openspec/changes/sf-cc/tasks.md:1-5,59-79` 与 `openspec/changes/sf-do/tasks.md:3-4,134-149` 明确把 recursive `sf_next`/`sf_cc_ref`/root `sf_cc_mref` 的 check 状态保留为历史事实并将 linked rewrite 标为未实现；这些 tasks 只作非规范线索，不能替代 code inspection。
- `test/async_sf_cc_SUITE.erl`、`test/async_sf_SUITE.erl` 的 old-shape assertions 是 target revision 的 observable tests，但相对于当前 normative inputs 已 stale；通过它们不能证明 generation-2 contract 完成。

### Inferences

- 从 `src/async_t.erl:232-249,431-461` 可推断当前 `(K,SfContext)` 已有传播通道，且 suspension writer 是唯一实际写 action entry 的路径；这只是 implementation fact 的组合，不决定 future representation。
- 从 `src/async_sf_transform.erl:13-102,338-405` 可推断 two-pass occurrence/digest machinery 可被后续 planner 作为现有 capability 边界读取；是否修改、保留或替换不在本 packet 决定。
- 从 current tests 与 `git diff --name-status` 的空 code/test delta 可推断 latest linked contract 尚未落入目标 revision 的实现/测试；该 inference 与 current-code no-match 和 changelog evidence 相互独立，但不扩展为实现方案。

### Unknowns

- `ContinuationRef` node registration/take、stable/ordinary/intermediate/outer classifier、reverse ownership index、shared-parent/cycle/orphan cleanup 在目标 revision 没有 current implementation evidence；后续 plan/task 必须冻结其 observable contract 与 ownership/lifecycle boundary。
- `#sf_callback{}` 的精确 shared header 字段、ordinary callback `parent_ref` 产生时机、bind_frame parent materialization 及 nested ordinary downstream node 的生命周期尚未由 current code 实现验证。
- direct `/2` ignored return、direct `/3` CleanState/NewState validation、async_r_t `/2` invalid return 在 linked node take 后的 exception/rollback boundary 尚未由 target implementation 验证。
- current manifest 从 path/async id 到 module-wide Block/StepName schema 的迁移细节、名称碰撞/名称长度错误、unnamed locator 的 manifest 位置尚未实现；当前 old tests 不能回答这些 unknowns。
- `case`/`receive`/`try` equality capture 与 `fun`/named-fun fresh shadowing 的 current helper implementation 未在本 packet 的重点 code path 中单独证明；已有 observable assertions 位于 `test/async_sf_SUITE.erl:273-302`，其是否满足最新 unified classifier 需要后续验证。
- compile、EUnit、Common Test、xref、Dialyzer、supported OTP matrix 在本次 context collection 未执行；因此不记录 pass/fail claim。

### Facts that must be frozen in plan/task

- normative-to-code ownership：`async_sf_cc_macro` 的 source validation、`async_sf_cc` 的 node lifecycle/dispatch/cleanup/inspection/failure、`async_t` 的 writer/router/context、`async_sf_runtime` 的 current-frame resolution、`async_util` 的 classifier/backend、`async_m` 的 facade；不得由 context packet 重新分配 owner。
- exact persistent shapes and reader boundary：ordinary/stable action entries、ordinary/intermediate/outer node value shapes、`parent_ref = none` chain end、absence-as-consumed、缺少 `parent_ref` 的旧 tuple 是否 reader-incompatible。
- exact normal/failure/exception ordering：preflight → take → direct cleanup → current execution 或 direct-parent forwarding；ordinary business Outcome 与 current continuation availability failure 的分类；callback-internal exception、invalid return、late reply、duplicate fail 的 non-rollback behavior。
- exact routing/ownership：ActionMRef、timer、monitor、AccRef、ContinuationRef、parallel reverse index 的各自 owner；message non-consumption；reply/timeout/DOWN consumption；shared parent、orphan cleanup、cycle/depth constraints。
- exact public/compatibility surface：sf-prefixed macro source restriction、ordinary/wait runner arguments/normalization/result、`run_sf_frame/4` 的 compatibility status、inspection summary privacy、active-failure result statuses。
- exact transform/manifest contract：`{Protocol,Block,StepName}` identity、`'$sf_<block>__<step>'/2` MFA、unnamed warning/fail-closed、minimal sorted capture schema、literal Vsn/`undefined`、two-pass diagnostic gate 与 no-generated-AST reanalysis。

## Context evidence request closure

- 当前 assignment 没有 `context-2-require-M.md` delta；requirements、capabilities、interfaces、callers、tests、bounded history、conflicts、unknowns、literal searches、locations、revisions 与 coverage limits 均已在本 packet 建立证据。
- 状态：`closed`（context collection closure）；未把任何 implementation task 或 architecture disposition 标记为完成。

## Evidence index

## Evidence E-1

- Claim: sf-cc 的 public runner、macro、descriptor、mode 与 ordinary compatibility requirements。
- Evidence class: `normative`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `### Requirement: Stable CC uses dedicated sf-prefixed runners`; `### Requirement: Macro accepts only explicit external callback source forms`; `### Requirement: sf_cc runner modes have distinct return contracts`
- Search method: `git show HEAD:openspec/changes/sf-cc/specs/sf-cc/spec.md | nl -ba | sed -n '1,167p'`
- Search scope: `openspec/changes/sf-cc/specs/sf-cc/spec.md`
- Location: `openspec/changes/sf-cc/specs/sf-cc/spec.md:5-165`
- Finding: requirements 固定三种 sf-prefixed API、external-fun AST gate、opaque Context、`/2|/3` mode semantics 和 ordinary runner compatibility。
- Limits: 规范要求不等于 target code 已实现。

## Evidence E-2

- Claim: sf-cc 的 linked node encoding、direct dispatch、exception、inspection/failure contract，以及 sf-do 对同一能力的依赖。
- Evidence class: `normative`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `### Requirement: Every action callback stores its current continuation and direct parent`; `### Requirement: One reference type addresses every continuation node`; `### Requirement: Dispatch consumes and cleans one node before invocation`; `### Requirement: Matcher failures are preflight-atomic`
- Search method: `git show HEAD:openspec/changes/sf-cc/specs/sf-cc/spec.md | nl -ba | sed -n '167,465p'`
- Search scope: `openspec/changes/sf-cc/specs/sf-cc/spec.md`
- Location: `openspec/changes/sf-cc/specs/sf-cc/spec.md:167-465`
- Finding: stable/ordinary entries 只有 current continuation + direct parent，node exactly-once，failure 逐层，summary sanitized，matcher mutation 前完成。
- Limits: 未定义 sf-migrate 的 converter return shape、mapping catalog 或 downgrade policy。

## Evidence E-3

- Claim: sf-do 的原 Monad/frame/lowering/identity/two-pass/manifest requirements。
- Evidence class: `normative`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `### Requirement: sf_do preserves the Monad type`; `### Requirement: Frame-aware Monad bind`; `### Requirement: Named async identity uses Protocol, Block and StepName directly`; `### Requirement: Astranaut compiler traversal`
- Search method: `git show HEAD:openspec/changes/sf-do/specs/sf-do/spec.md | nl -ba | sed -n '1,567p'`
- Search scope: `openspec/changes/sf-do/specs/sf-do/spec.md`
- Location: `openspec/changes/sf-do/specs/sf-do/spec.md:5-567`
- Finding: `sf_do` 是原 Monad，frame 是 `(Input, Env) -> Monad`，identity/MFA 不含 hash/AsyncId，transform 需要冻结 plan 后 rewrite。
- Limits: sf-do 依赖 sf-cc 的 runner/store/dispatch，不自行拥有 final completion。

## Evidence E-4

- Claim: target header 暴露了新宏，但 shared callback record 仍为旧 CcRef encoding。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `-use_macro({async_sf_cc_macro, run_sf_cc/3`; `-record(callback`; `sf_cc_mref`
- Search method: `git show HEAD:include/async.hrl | nl -ba`; `git show HEAD:include/async_internal.hrl | nl -ba`; `git grep -n -F 'parent_ref' HEAD -- include/async*.hrl src/async*.erl src/monad_frame.erl test/async_sf*.erl test/async_t_SUITE.erl`
- Search scope: `include/async*.hrl` 与用户指定代码/测试 scope；第三个命令 no-match，exit `1`。
- Location: `include/async.hrl:1-5`; `include/async_internal.hrl:1-7`
- Finding: `async.hrl` 注册三个 macro；internal header 只有 `#callback{cc,acc_ref,sf_cc_mref}`，整个 bounded scope 没有 `parent_ref` literal。
- Limits: no-match 只证明 exact literal absence，不证明同义字段或 runtime semantics。

## Evidence E-5

- Claim: `async_m` public facade 与现有 `run_sf_frame/4` compatibility path。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `-export([run_sf_cc/3, run_with_sf_cc/5, exec_sf_cc/5])`; `run_sf_frame`; `async_t:run_sf_cc`
- Search method: `git grep -n -E 'run_sf_cc|run_with_sf_cc|exec_sf_cc|run_sf_frame' HEAD -- src/async_m.erl src/async_t.erl include/async*.hrl`
- Search scope: `src/async_m.erl`, `src/async_t.erl`, `include/async*.hrl`
- Location: `src/async_m.erl:34-37,206-223`; `src/async_t.erl:799-809`
- Finding: facade exports new runners and delegates to async_t；同时保留 `run_sf_frame/4`，它从 descriptor `mfa` 重建 external fun。
- Limits: public facade 的存在不证明 macro source restriction 或 linked lifecycle。

## Evidence E-6

- Claim: current async action context、bind_frame、ordinary bind 与 promise writer 的实际行为。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `type sf_context`; `bind_frame(Action`; `async_t(fun(K, SfContext)`; `#callback{cc = NK, acc_ref = AccRef, sf_cc_mref = CcRef}`
- Search method: `git grep -n -E 'sf_context|bind_frame\(|async_t\(fun\(K, SfContext\)|#callback\{' HEAD -- src/async_t.erl src/async_r_t.erl src/monad_frame.erl include/async*.hrl`
- Search scope: `src/async_t.erl`, `src/async_r_t.erl`, `src/monad_frame.erl`, `include/async*.hrl`
- Location: `src/async_t.erl:13-17,232-252,431-461`; `src/async_r_t.erl:209-243`
- Finding: context 已显式作为 `(K,SfContext)` 传播；`bind_frame/4` 新建 frame；promise writer 保存 old `sf_cc_mref`，并把 AccRef/CcRef 作为分离 reader local。
- Limits: current context 没有规范的 `parent_ref`，且 writer 不能证明 nested direct-parent semantics。

## Evidence E-7

- Claim: runner 和 nested path 仍注册 CcRef terminal、构造 recursive `sf_next`。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `run_with_descriptor`; `async_sf_cc:new_ref`; `async_sf_cc:terminal`; `async_sf_runtime:new_next`
- Search method: `git grep -n -E 'run_with_descriptor|async_sf_cc:new_ref|async_sf_cc:terminal|async_sf_runtime:new_next' HEAD -- src/async_t.erl src/async_sf_runtime.erl src/async_sf_cc.erl`
- Search scope: `src/async_t.erl`, `src/async_sf_runtime.erl`, `src/async_sf_cc.erl`
- Location: `src/async_t.erl:811-836`; `src/async_sf_runtime.erl:27-30,69-75,89-106`
- Finding: common runner 以 CcRef 写 `sf_cc_ref` descriptor，stable downstream 由 recursive `new_next(Frame, Downstream)` 连接，terminal 仍是 CcRef map。
- Limits: exact runtime data shapes 由 current source/test 证明，不是 latest normative shape。

## Evidence E-8

- Claim: callback store classifier/backend 与 ordinary action callback 的当前边界。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `callback_exists`; `decode_callback(#callback`; `callback_value_kind`; `fun_availability`
- Search method: `git show HEAD:src/async_util.erl | nl -ba`
- Search scope: `src/async_util.erl:23-123`
- Location: `src/async_util.erl:23-64,76-123`
- Finding: map/list/dict backend 共用 store/find/remove/to_list；decoder 识别 `#callback{cc,acc_ref,sf_cc_mref}`，classifier 只把它标成 `action_callback`，另识别 `sf_cc_ref`。
- Limits: no `#sf_callback{}`、intermediate/outer/accumulator named classification；`fun_availability/1` 只提供 local/external MFA availability helper。

## Evidence E-9

- Claim: current `async_sf_cc` owns CcRef prepare/cleanup/invocation/inspection/active-failure。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `prepare(CcRef`; `remove_owned`; `apply_available_callback`; `summary(MRef`; `commit_failures`
- Search method: `git show HEAD:src/async_sf_cc.erl | nl -ba`
- Search scope: `src/async_sf_cc.erl:21-326`
- Location: `src/async_sf_cc.erl:31-120,122-161,163-241,243-326`
- Finding: `prepare/4` 删除 CcRef、清理 owned callbacks/resources/accumulators 后 invoke；summary 暴露 old recursive frames/root CcRef；active failure 按 protected CcRef `lists:usort/1` 去重。
- Limits: current behavior conflicts with direct-node take/forwarding and current-only summary; no linked node executor。

## Evidence E-10

- Claim: current runtime frame encoding/resolution/failure boundary。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `-type sf_continuation`; `continue_valid`; `resolve_manifest_frame`; `complete_current`
- Search method: `git show HEAD:src/async_sf_runtime.erl | nl -ba`
- Search scope: `src/async_sf_runtime.erl:16-308`
- Location: `src/async_sf_runtime.erl:16-30,69-121,130-284,286-308`
- Finding: `sf_continuation()` 是 `sf_next` recursive downstream 或 `sf_cc_ref`；frame 使用 `block_path`/`async_id`/`env`；resolution failure 通过 current `get_sf_cc_ref/1` terminal completion。
- Limits: current owner/Vsn manifest checks cannot establish new `{Protocol,Block,StepName}` identity or direct parent dispatch。

## Evidence E-11

- Claim: current transform has Astranaut two-pass, source diagnostics, marker pairing and capture/lowering capability。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `parse_transform`; `rewrite_module`; `pair_step_markers`; `required_captures`; `frame_call`; `generated_frame`
- Search method: `git grep -n -E 'parse_transform|rewrite_module|pair_step_markers|required_captures|frame_call|generated_frame' HEAD -- src/async_sf_transform.erl`
- Search scope: `src/async_sf_transform.erl:13-1128`
- Location: `src/async_sf_transform.erl:13-102,367-405,425-700,804-910`
- Finding: Pass 1 collect 与 Pass 2 mapfold rewrite 分离，保存 occurrence plan/digest；marker/qualifier/capture 分析与 `bind_frame`/`then_frame` generated function 已有实现。
- Limits: generic whole-project transform behavior 未收集；只记录 sf-do direct path。

## Evidence E-12

- Claim: current transform identity/context/manifest 使用旧 derived fields。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `sf_unnamed_local`; `crypto:hash`; `Function = list_to_atom`; `block_path`; `async_id`; `blocks => Blocks`
- Search method: `git grep -n -E 'sf_unnamed_local|crypto:hash|Function = list_to_atom|block_path|async_id|blocks => Blocks' HEAD -- src/async_sf_transform.erl src/async_sf_runtime.erl test/async_sf_SUITE.erl`
- Search scope: `src/async_sf_transform.erl`, `src/async_sf_runtime.erl`, `test/async_sf_SUITE.erl`
- Location: `src/async_sf_transform.erl:702-736,985-1018,1111-1117`; `src/async_sf_runtime.erl:202-275`
- Finding: named/unnamed identity 由 SHA/hex/hash 生成，frame/manifest 仍写 `block_path` 与 `async_id`，runtime 用这些字段 lookup current manifest。
- Limits: this is exact current-code finding；不推断未来 migration policy。

## Evidence E-13

- Claim: macro implementation matches current external-fun source gate but lacks runtime lifecycle evidence。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `external_fun_arity`; `remote_call`; `astranaut_return:formatted_error`
- Search method: `git show HEAD:src/async_sf_cc_macro.erl | nl -ba`
- Search scope: `src/async_sf_cc_macro.erl:8-53`
- Location: `src/async_sf_cc_macro.erl:11-51`
- Finding: accepted AST 仅按 `function` fun form 与 arity，success 生成普通 remote runtime call，failure 使用 Astranaut formatted error。
- Limits: `external_fun_arity/1` 只做 AST shape/arity 检查；runtime availability 由 `async_util:fun_availability/1` 另行处理。

## Evidence E-14

- Claim: current sf-cc tests observe old runner/store/cleanup/inspection/failure behavior。
- Evidence class: `test`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `all()`；`pending_store_and_scopes`; `completion_cleanup_exactly_once`; `matcher_privacy_and_atomicity`; `active_fail_dedupe`
- Search method: `git grep -n -E '^[a-zA-Z][a-zA-Z0-9_]*\(_Config\)|sf_cc_ref|sf_cc_mref|block_path|async_id' HEAD -- test/async_sf_cc_SUITE.erl`
- Search scope: `test/async_sf_cc_SUITE.erl:14-473`
- Location: `test/async_sf_cc_SUITE.erl:14-25,43-189,191-335,367-408`
- Finding: tests覆盖 runner modes、AccRef/CcRef、cleanup、message/reply/timeout/DOWN、exception、matcher atomicity、active fail；同时显式断言 `sf_cc_ref`、`sf_cc_mref`、recursive frames 和 old tuple。
- Limits: old assertions 不能作为 generation-2 linked contract 完成证明；没有 `parent_ref`/reverse-index/node-take tests。

## Evidence E-15

- Claim: current sf-do tests cover source/lowering/capture/old frame lifecycle and old identity。
- Evidence class: `test`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `source_api_diagnostics`; `same_monad_lowering`; `capture_compatibility`; `identity_manifest`; `continuation_kinds`
- Search method: `git grep -n -E 'source_api_diagnostics|same_monad_lowering|capture_compatibility|identity_manifest|continuation_kinds|sf_cc_ref' HEAD -- test/async_sf_SUITE.erl`
- Search scope: `test/async_sf_SUITE.erl:15-340,342-403,427-478`
- Location: `test/async_sf_SUITE.erl:15-37,56-167,169-209,236-340,342-403,427-478`
- Finding: observable tests覆盖 grammar、two-pass diagnostics、same-Monad semantics、nested/capture/lifecycle/structural failure；old representation assertions 包括 `[sf_next, sf_next, sf_cc_ref]`、SHA/hex MFA 和 `async_id` manifest。
- Limits: current test fixtures are not a test of latest direct-parent linked model。

## Evidence E-16

- Claim: ordinary async caller/regression coverage exists for message/timeout/DOWN/parallel/accumulator/store behavior。
- Evidence class: `test`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `test_async_t_with_timeout`; `test_async_t_with_message`; `test_async_t_pmap`; `test_local_acc_ref`; `test_async_t_local_acc_ref`
- Search method: `git grep -n -E 'test_async_t_with_timeout|test_async_t_with_message|test_async_t_pmap|test_local_acc_ref|test_async_t_local_acc_ref|run_with_cc' HEAD -- test/async_t_SUITE.erl`
- Search scope: `test/async_t_SUITE.erl:111-878`
- Location: `test/async_t_SUITE.erl:111-128,152-228,274-349,514-817,829-878`
- Finding: ordinary lifecycle、message、timeout、parallel map、accumulator 与 wait paths 有代表性 coverage。
- Limits: 未覆盖 linked node registration/take、`parent_ref` forwarding、reverse ownership index、new summary/status schema。

## Evidence E-17

- Claim: bounded history shows implementation baseline/test coverage predates latest linked contract, and latest two changes are document-only in the scoped code/test paths。
- Evidence class: `history`
- Bound revision: `6dac66f7da63dda3ef3c2e00377c8cfe3023649c^..0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `feat: migrate sf-do implementation onto clean history`; `test(sf-cc): close deferred fixtures and coverage`; `sf-cc-fix-cleanup-before-invoke`; `docs-freeze-linked-continuation-contract`; `docs: simplify sf continuation identity`
- Search method: `git log --date=short --format='%H %ad %s' -20 -- <bounded paths>`；`git show --name-status <commit>`；`git diff --name-status aec8422cf1e3b8c28c2b792ae5195999ee69a661 0adf22a22f20c45fff192d52398f23f83bf4548c -- <scoped code/test paths>`
- Search scope: 当前 control repository；仅 bounded paths，未读 `sf-migrate` 或其他 repository。
- Location: commits `6dac66f7da63dda3ef3c2e00377c8cfe3023649c`、`d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03`、`7fc7db0896a24556bd83c0b59ae60be9e35d0ba0`、`8bbefd3f8d6c45a2cf4da97951c501d53963df62`、`aec8422cf1e3b8c28c2b792ae5195999ee69a661`、`0adf22a22f20c45fff192d52398f23f83bf4548c`
- Finding: `6dac` 建立 old sf-do runtime/transform baseline，`d336` 建立 old sf-cc tests，`7fc` 修 old CcRef cleanup，`8bb` 修 transform diagnostics；`aec` 与 `0adf` 仅改 OpenSpec 文档，scoped code/test diff 为空。
- Limits: history 只说明 bounded evolution，不把任何旧 commit 的 design 当作 current normative contract。

## Evidence E-18

- Claim: required new linked symbols are absent from the exact bounded current scope。
- Evidence class: `no-match`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `parent_ref`; `#sf_callback`; `dispatch_continuation`; `continuation_node`; `ordinary_continuation`
- Search method: `git grep -n -F 'parent_ref' HEAD -- <scope>`；`git grep -n -F '#sf_callback' HEAD -- <scope>`；`git grep -n -E 'dispatch_continuation|continuation_node|ordinary_continuation' HEAD -- <scope>`
- Search scope: `<scope>` 精确为 `include/async*.hrl src/async.erl src/async_m.erl src/async_t.erl src/async_r_t.erl src/async_r_m.erl src/async_util.erl src/async_sf_cc.erl src/async_sf_cc_macro.erl src/async_sf_runtime.erl src/async_sf_transform.erl src/monad_frame.erl test/async_sf*.erl test/async_t_SUITE.erl`
- Location: no-match，三个命令均 exit `1`；补充 exact query `git grep -n -F '#sf_callback' HEAD -- <scope>` 同样 no-match。
- Finding: target code/test scope 没有 new direct-parent record/node primitive literal。
- Limits: no-match 不排除未来 generated code、依赖库或未授权 scope；不读取这些 scope。

## Evidence E-19

- Claim: current derived identity and old recursive field literals remain present。
- Evidence class: `current-code`
- Bound revision: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Key search strings: `sf_unnamed_local`; `crypto:hash`; `sf_cc_ref`; `sf_next`; `sf_cc_mref`; `downstream`
- Search method: `git grep -n -E 'sf_unnamed_local|crypto:hash|sf_cc_ref|sf_next|sf_cc_mref|downstream' HEAD -- <scope>`
- Search scope: `src/async_sf_transform.erl src/async_sf_runtime.erl src/async_sf_cc.erl src/async_t.erl include/async_internal.hrl test/async_sf*.erl`
- Location: `src/async_sf_transform.erl:711-735`; `src/async_sf_runtime.erl:27-30,69-121`; `src/async_sf_cc.erl:33-46,255-261`; `src/async_t.erl:445,816,833`; tests `test/async_sf_SUITE.erl:174-184,240-241`。
- Finding: old identity/terminal/root-edge symbols are active in implementation and tests。
- Limits: symbol presence does not by itself quantify all behavior; behavior claims are separately bounded by E-6–E-16。

## Coverage limits

- Relevance boundary: 只收集 Goal `sf-cc` 直接涉及的 sf-cc/sf-do completion、callback store、frame/runtime、transform/macro、ordinary caller 与指定 test evidence；没有建立 whole-project map。
- Included: `include/async*.hrl`；`src/async.erl`、`async_m.erl`、`async_t.erl`、`async_r_t.erl`、`async_r_m.erl`、`async_util.erl`、`async_sf_cc.erl`、`async_sf_cc_macro.erl`、`async_sf_runtime.erl`、`async_sf_transform.erl`、`monad_frame.erl`；`test/async_sf*.erl`、`test/async_t_SUITE.erl`；sf-cc/sf-do OpenSpec current documents；bounded history listed in E-17。
- Excluded: `openspec/changes/sf-migrate/**`、`sf-migrate` implementation/test；whole-project unrelated modules；任何 prior-generation `context-1.md`、`plan-1.md`、`lanes-1.md`、task/review/report/status/diagnostic/error artifacts；未读取 `sf-migrate` 作为本 generation 输入。
- Unavailable: 本次未执行 compile/EUnit/Common Test/xref/Dialyzer/OTP matrix；没有运行时外部模块升级环境或 sf-migrate converter evidence；未对未授权 repository 查询。
- Invalidated evidence IDs: none
- Provisional or concurrent worktree delta excluded from evidence: none；collection 前后 `git status --short` 均为空，且 target before/after 均为 `0adf22a22f20c45fff192d52398f23f83bf4548c`。
- Dirty paths classified unrelated: none；未发现 dirty path。
- User-declared unrelated/not-to-commit paths: none
