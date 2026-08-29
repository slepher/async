# sf-cc Generation 2 Plan

- Goal: sf-cc
- Generation: 2
- Context: context-2.md
- Evidence target: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Normative inputs: `openspec/changes/sf-cc/**`, `openspec/changes/sf-do/**`
- Planning status: draft

## 行为基线

本 generation 把 `sf-cc` 与 `sf-do` 的 linked-continuation 合同作为一个 acceptance unit。所有 public runner——`wait/1`、`wait_t/2`、ordinary runners、`run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5`——必须注册并消费同一种 `ContinuationRef` node。sf-prefixed macro 仍只接受显式 external fun source form，runtime 原样保存 fun term 与 opaque Context，并保持 async `/2`、direct-state `/2`、direct-state `/3` 三种 return contract；ordinary 和 wait API 的参数、normalization、正常结果及 anonymous callback source compatibility 不变。

ordinary action 的完整 entry 是 `#callback{cc,acc_ref,parent_ref,...}`，stable action 的完整 entry 是 `#sf_callback{frame,acc_ref,parent_ref,timer_ref,...}`。ActionMRef、timer、monitor 与 AccRef 仍属于 action lifecycle；current continuation 只有一个 direct `parent_ref`。frame、ordinary continuation 与 outer completion 都以同一种 reference() key 存于 existing callbacks dictionary，value shape 决定 executor，`parent_ref = none` 结束链，absence 表示 consumed，不留 tombstone。

统一 dispatch 对一个 node 严格执行 preflight → take → direct resource cleanup → invoke/forward。每个 node exactly-once 且 take 后不可 rollback/retry。ordinary business Outcome、timeout、DOWN 与 pattern failure 先进入 current `cc`/frame；只有精确归属正在调用的 current continuation 的 availability `badfun`/`undef`，或 frame protocol/shape/manifest/MFA/migration structural failure，才生成 `{error,{sf_runtime,Reason}}` 并只 dispatch direct parent。callback/effect 内部异常保留原生 identity 与传播语义。

inspection/failure 只公开 current-node sanitized summary；matcher 按 RequestMRef term order 在 mutation 前完成 snapshot/sort/preflight。active failure 使用 `{error,{sf_aborted,Reason}}`，按 selected Ref 顺序提交并返回 `forwarded | handled | failed | unhandled | ordinary`；ordinary selected callback 保持 pending。summary 不得泄露 Env、State、timer、monitor、accumulator、continuation payload、outer callback 或 Context。

`sf_do` 保持原 Monad 类型，生成 `(Input, Env) -> Monad` frame，并使用 `monad_frame:bind_frame/4`、`then_frame/4`；local `return/1`、`fail/1` 按 lexical Monad rewrite，final action 直接返回且不生成 frame。`(K,SfContext)` 显式传播，`bind_frame` 只物化 direct parent；nested representation 是 `InnerFrame + OuterRef`。named identity 精确为 `{Protocol,Block,StepName}`，generated MFA 为 `'$sf_<block>__<step>'/2`；identity 不含 AsyncId/hash/Action AST/source index/lexical parent/capture layout。unnamed boundary 仅允许 ordinary execution/same-version resume，migration 前 fail closed。compiler Pass 1 冻结 occurrence/digest/plan 并累计 diagnostics；有 error 不进入 Pass 2，Pass 2 只消费 frozen plan；manifest 只公开 current owner/literal Vsn/identity/MFA/required captures。

## Scope

包含：shared callback records；callbacks store classifier/backend 与 reverse ownership metadata；public facade、macro expansion 与所有 runner construction；action suspension writer 和 reply/message/timeout/DOWN router；unified node dispatch、cleanup、inspection、active failure；current-frame resolution；`sf_do` transform、identity、manifest、capture/lowering；与上述合同直接对应的 document-driven public black-box tests 和 ordinary regressions。

不包含：`sf-migrate` converter、mapping catalog、downgrade policy或任何 `openspec/changes/sf-migrate/**`；历史 callback tuple 的 runtime reader；新的 public migration API；对 unrelated modules 的重构；generation-1 artifact；status、OpenSpec `tasks.md`、stage/commit、lane provisioning 或实现执行。

## Acceptance semantics

1. R1–R19 按 `openspec/changes/sf-cc/specs/sf-cc/spec.md` 的前十九个 Requirement 顺序形成可追踪 acceptance matrix：dedicated runners；every-runner outer node；macro source gate；descriptor fun/Context；runner mode returns；action current/direct parent；AccRef independence；explicit context；single Ref type；dispatch ordering；exactly-once；message retention；one-level availability forwarding；linked nested frames；reader/classifier distinction；exact final availability；sanitized inspection；atomic matcher；active-failure order/status。
2. `Module version metadata uses literal module vsn terms` 与全部 `sf-do` linked contract 作为同一 gate：same Monad、bind/then frames、explicit context、single stable encoding、Monad-returning frames、lexical return/fail、nested composition/pattern semantics、ordinary timeout/DOWN、one-level structural failure、minimal Env、direct named identity/MFA、owner resolution、manifest、two-pass diagnostics、shared lifecycle、all runners 与 additive ordinary compatibility。
3. immediate 与 suspended、ordinary 与 stable、async 与 direct-state、message/reply/timeout/DOWN、sequential/nested/parallel/shared-parent、map/list/dict callbacks backends 均以 public observable behavior 验证；不把 internal record/map layout本身当作唯一完成证据。
4. direct callback 在 current node 与 direct resources 已移除后的 CleanState 上执行；`/2` return ignored，`/3` 只接受 compatible State，invalid return 发生在 node consumed 后。late reply、duplicate fail、callback crash、invalid return 均不得恢复或重试 node。
5. external callback availability 先用 `code:ensure_loaded/1` 与 `erlang:function_exported/3` 精确判定；只捕获 term 精确相同的 `error:{badfun,Callback}`。进入用户 callback 后的 `badfun`、`undef` 和其他异常原样传播。
6. new document-driven tests 不重复 E-14–E-16 已覆盖的旧 runner/source/ordinary baseline；保留可继续成立的 assertions，替换 stale `sf_cc_ref`/`sf_next`/`sf_cc_mref`/old summary/SHA identity assertions，并只新增 linked-contract 缺口及一组 ordinary cross-lifecycle regression。
7. `rebar3 compile`、EUnit、focused/common Common Test、`rebar3 xref`、`rebar3 dialyzer` 与 repository-supported OTP matrix 均通过；任何 product defect 不能作为 transitional test failure 接受。

## Persistent representations and lifecycle decisions

shared header 冻结以下 semantic shape；允许只为真实 action lifecycle 追加 monitor/resource metadata：

```erlang
-record(callback, {cc, acc_ref, parent_ref}).
-record(sf_callback, {frame, acc_ref, parent_ref, timer_ref = undefined}).

#{kind := ordinary_continuation,
  protocol := 1,
  callback := fun(),
  parent_ref := reference() | none}

#{kind := sf_cc,
  protocol := 1,
  mode := async_r_t | direct_state,
  callback := fun(),
  context := term(),
  parent_ref := none}
```

`SfContext` 冻结为 `undefined | #{current := {frame,SfFrame} | {ordinary,Fun} | {ref,ContinuationRef}, parent_ref := ContinuationRef | none, runner := fun()}`。ordinary/library outer completion 复用 `sf_cc` node executor 与 descriptor boundary，但 macro external-source guarantee 只属于 sf-prefixed APIs；`run_sf_frame/4` 作为 same-version additive compatibility facade 保留并路由到统一 lifecycle，新 transform 不生成或依赖它，它不提供 named migration identity。

parallel cancellation 的 reverse index 由 `async_sf_cc` 私有拥有，使用 callbacks dictionary 中与 reply key 不冲突的 namespaced metadata entry，从 outer/direct owner 指向去重且稳定排序的 child Ref 集合。该 metadata 不参与 continuation execution、不是 external reply target、不得复制 root/final Ref；registration/take 同步维护，orphan 清理幂等，shared parent 的第二次到达返回 unhandled。合法构造只允许新 Ref 指向已存在 direct parent；reader 对 self-edge、cycle、超过共享有限 depth bound、orphan 或 invalid shape 返回 structural failure，不猜测 root。dispatch 与显式 chain inspection 共用同一 cycle/depth guard，默认 inspection 仍只返回 current summary。

## Capability dispositions

| Capability | Disposition | 唯一 owner | Evidence | 冻结设计与 retired callers/symbols |
|---|---|---|---|---|
| sf-prefixed source validation | reuse | `async_sf_cc_macro` | E-4, E-13 | 保留 external fun `/2|3` AST gate、source-position diagnostics 与 AST-preserving expansion；runtime lifecycle 不移入 macro。无 retired public symbol。 |
| public runner/inspection facade | extend | `async_m` | E-5, E-17 | 保持 signatures/result projection，所有 runner 改接 common outer-node core；`run_sf_frame/4` 仅作上述 additive compatibility。退役绕过 unified lifecycle 的 facade caller。 |
| callback records | replace | `include/async_internal.hrl` | E-4, E-18, E-19 | `#callback.sf_cc_mref` 被 `parent_ref` 替代并新增完整 `#sf_callback{}`；旧无 `parent_ref` tuple reader-incompatible。退役全部 `sf_cc_mref` writers/readers/tests。 |
| shared store backend/classifier | extend | `async_util` | E-8, E-18 | 一个 classifier 区分 stable action、ordinary action、intermediate node、outer node、accumulator/other；map/list/dict 同义，router拒绝 node reply。退役 `sf_cc_ref` decoder/classifier 与 ordinary-only `callback_exists/1` assumptions。 |
| node constructors、dispatch、cleanup、reverse index、inspection/failure | replace | `async_sf_cc` | E-9, E-17–E-19 | 建立单一 `dispatch_continuation/4`，node-local preflight/take/cleanup/invoke，current-only summary和selected-order failure。退役 CcRef `new_ref/2`/`terminal/1`、root scan/remove-owned、CcRef dedupe 与 old summary helpers及其 callers。 |
| action writer/router/context/materialization | extend | `async_t` | E-6, E-7 | writer按实际 `K =:= runner` 写 stable或ordinary完整entry；所有 runners注册outer node；reply/message/timeout/DOWN只解码action，`bind_frame`物化一层parent。退役 `get_sf_cc_ref` writer依赖、`run_with_descriptor` CcRef terminal 与 `new_next/2` callers。 |
| AccRef/current-context reader scopes | replace | `async_r_t` | E-6 | 保留 `get_local_ref/1`/`local_ref/3` 的 AccRef authority，第二local改为显式 current/parent context，不再传播root CcRef。退役 `get_sf_cc_ref/1`、`local_sf_cc_ref/3`及 callers。 |
| current frame resolution/invocation | replace | `async_sf_runtime` | E-10, E-12, E-19 | 只接收current `sf_frame + parent_ref`；逐owner/literal Vsn/manifest/MFA/capture resolve，returned Monad交回 runtime；structural failure只forward一层。退役 recursive `sf_next`、`sf_cc_ref`、`new_next/2`、downstream traversal、terminal completion 与 block_path/async_id lookup。 |
| frame typeclass facade | reuse | `monad_frame` | E-6 | 保留 `bind_frame/4`、`then_frame/4`、`default_then_frame/4` 和 original Monad return；不新增第二套 sequencing abstraction。无 retired public symbol。 |
| sf-do source analysis/lowering | extend | `async_sf_transform` | E-11, E-15 | 复用 Astranaut collect/rewrite、lexical occurrence、marker、capture、bind/then与diagnostic ownership；Pass 1冻结完整plan且error gate Pass 2，generated AST不再分析。退役 Pass 2重新决策入口。 |
| named identity/MFA/manifest | replace | `async_sf_transform` | E-12, E-17, E-19 | 直接 `{1,Block,StepName}` 与 `'$sf_<block>__<step>'/2`，module-wide collision/length diagnostics，minimal sorted capture atoms，manifest仅current owner/Vsn/identity/MFA/captures；unnamed migration fail closed。退役 hash/hex/AsyncId/block_path/source-index/lexical-parent/capture-layout identity及 old manifest readers/tests。 |
| final completion ownership | reuse | `async_sf_cc` | E-1–E-3 | `sf-do` 不定义descriptor、runner或root lookup；只生成frame/manifest并依赖direct parent dispatcher。退役 `async_sf_runtime:complete_current/2` terminal CcRef completion。 |

## Designed errors and exception boundaries

transform diagnostics 由 `async_sf_transform` 产生 raw semantic reason，位置与呈现仍由 Astranaut/compiler owner 补充；Pass 1 聚合独立 violations，禁止同根因派生噪音，unknown direct formatter reason 保持 `function_clause`。runtime structural failure 由 current-frame/continuation owner识别并规范化为 `{error,{sf_runtime,Reason}}`；active abort 使用 `{error,{sf_aborted,Reason}}`；matcher non-boolean 使用 `{invalid_matcher_return,Value}`。这些 designed outcomes 不借异常作正常控制流。

expected runtime exception 只在最近且能语义归属的 invocation boundary 捕获：current ordinary fun 的 term-exact `badfun`、external final callback 的 pre-apply availability与 term-exact `badfun`。其他 load error、callback/effect internal `badfun`/`undef`、matcher exception、invalid callback business exception 原样传播；cleanup已提交且node不回滚。

## OpenSpec topology exception

存在一个有理由的 topology exception：`openspec/changes/sf-cc/**` 与 `openspec/changes/sf-do/**` 不各建一对 parent，而合并为一个 `task-coding-2` 与一个 matching `task-test-2`。原因不是文件数或规模，而是 sf-do 的 frame writer/resolver 与 sf-cc 的 shared store/dispatcher 共用同一 ContinuationRef lifecycle、exactly-once take、direct-parent forwarding、reverse ownership index 与 exception classification；拆成两个 coding parent 会产生无法独立接受的中间表示或并行 lifecycle owner。`sf-cc` 仍唯一拥有 completion，`sf-do` 仍唯一拥有 lowering/identity/manifest，任务合并不合并 semantic owner。

## task-coding-2

一个 hard、single-superior coding parent，在固定 coding lane 上完成 shared representation、runner/store/dispatch/runtime/transform 的一次性 linked rewrite。其完成条件是 production-owned compile/xref 与 formatter unit baseline通过，并提交可供 matching test parent验证的 candidate；已声明的旧 shape Common Test transition只由 `task-test-2` 关闭。

## task-test-2

一个 hard、single-superior document-driven test parent，在固定 integrate lane 合并 `task-coding-2` candidate 后执行。它以 public APIs、generated fixture modules 与 observable State/result/effect 为边界建立 R1–R19 + sf-do linked-contract matrix，迁移 stale assertions，保留 ordinary regressions，运行完整 acceptance commands；它不把 private helper或record layout的直接断言当成唯一证明。

