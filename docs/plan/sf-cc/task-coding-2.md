# task-coding-2 Contract

- Task: task-coding-2
- Phase artifact: task-coding-2.md
- Bundle generation: 2
- Goal: sf-cc
- Context: context-2.md
- Normative inputs: `openspec/changes/sf-cc/**`, `openspec/changes/sf-do/**`
- Evidence target: `0adf22a22f20c45fff192d52398f23f83bf4548c`
- Mode: hard
- Mode basis: ContinuationRef registration/take、action routing、frame resolution、reverse ownership 与 generated frame identity 是跨 `async_sf_cc`、`async_t`、`async_r_t`、`async_util`、`async_sf_runtime`、`async_sf_transform` 的同一 lifecycle/concurrency/shared invariant；分离 worker 会留下双 encoding、双 dispatcher 或无法单独接受的 transition。
- Execution strategy: single-sup
- Worker role: sup-coding-worker
- Lane: coding
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-cc-generation-2-coding
- Base: 0adf22a22f20c45fff192d52398f23f83bf4548c
- Dependencies: none
- Covers: none
- Plan: plan-2.md
- Lane manifest: lanes-2.md

## Bounded goal

在 owned production paths 内一次性实现 generation-2 linked-continuation production candidate：所有 public runners 进入统一 ContinuationRef lifecycle；ordinary/stable callbacks 保存 current continuation 与 direct parent；`async_sf_cc` 逐 node exactly-once dispatch；`async_sf_runtime` 只处理 current frame；`sf_do` 生成 direct named identity/MFA、current manifest 与 frozen two-pass plan。保持 ordinary/wait observable contract、sf-prefixed source gate、Monad semantics、action resource lifecycle 和 unrelated behavior。

不实现或读取 `sf-migrate`；不编辑测试、OpenSpec、workflow/status、generation-1 artifacts；不运行 integration/full Common Test；不 stage/commit/merge/provision。

## Normative excerpts

以下是本任务必须原样遵守的 representation 与 dispatch contract（`openspec/changes/sf-cc/implementation.md:31-189`，并由 context-2.md E-1–E-3 绑定）：

```erlang
-record(callback, {
    cc,
    acc_ref,
    parent_ref
}).

-record(sf_callback, {
    frame,
    acc_ref,
    parent_ref,
    timer_ref = undefined
}).

-type continuation_ref() :: reference().

-type continuation_node() ::
    #sf_callback{}
  | #{kind := ordinary_continuation,
      protocol := 1,
      callback := fun(),
      parent_ref := continuation_ref() | none}
  | #{kind := sf_cc,
      protocol := 1,
      mode := async_r_t | direct_state,
      callback := fun(),
      context := term(),
      parent_ref := none}.
```

```erlang
undefined
| #{current := {frame, SfFrame} | {ordinary, Fun} | {ref, ContinuationRef},
    parent_ref := ContinuationRef | none,
    runner := fun()}.
```

```erlang
dispatch_continuation(Ref, Outcome, OffsetOrLens, State) ->
    {ok, DispatchValue, NewState}
  | {unhandled, State}
  | {error, Error, ConsumedState}.
```

每个 dispatch 的 exact order 是 preflight current node shape/direct cleanup targets → take node → cancel/demonitor/remove direct resources → obtain CleanState → invoke current executor或forward direct parent。take 后的 callback crash、invalid return、late reply、duplicate fail 或 structural failure不得恢复、retry或root-search。

`sf_do` generated frame 必须是 `(Input, Env) -> remainder Monad`；non-final bound/unbound qualifier分别调用 `monad_frame:bind_frame/4` 与 `then_frame/4`，final action不生成frame。named identity精确为 `{Protocol,Block,StepName}`，MFA精确为 `{Module,'$sf_<block>__<step>',2}`；manifest只包含current owner、literal Vsn/`undefined`、identity、MFA、排序后的required capture atoms。Pass 1冻结occurrence/digest/marker pairing/identity/captures/generated-name/manifest plan并累计diagnostics；存在error时不得初始化Pass 2，Pass 2只按lexical occurrence id消费frozen plan。

## Context evidence

- E-4/E-18：shared header 仍只有 `#callback{cc,acc_ref,sf_cc_mref}`，bounded scope 无 `parent_ref`、`#sf_callback{}`、`dispatch_continuation`。
- E-6/E-7：`(K,SfContext)` 与 frame adapter 已存在，但 writer/runner 仍传播 CcRef，nested path仍构造 recursive `new_next/2`。
- E-8/E-9：`async_util` 已提供 map/list/dict backend，`async_sf_cc` 已拥有旧 prepare/cleanup/inspection/fail boundary；这些是 replace/extend seam，不是新合同完成证据。
- E-10/E-12/E-19：runtime/transform仍使用 `sf_next`、`sf_cc_ref`、`block_path`、`async_id`、SHA/hex identity 与 terminal CcRef completion。
- E-11：transform 已有 Astranaut collect/rewrite、occurrence plan、marker/capture 与 frame lowering，可 extend 而不新建第二 compiler pipeline。
- E-14–E-16：现有 tests 固定旧 shape；本 task 不编辑它们，明确的 transition 由 covering `task-test-2` 关闭。
- E-17：latest linked contract 是 document-only delta；E-18 的 required symbols no-match，不能把 current baseline 当作兼容 authority。

## Owned paths

- `include/async_internal.hrl`
- `include/async.hrl`
- `src/async_m.erl`
- `src/async_t.erl`
- `src/async_r_t.erl`
- `src/async_util.erl`
- `src/async_sf_cc.erl`
- `src/async_sf_cc_macro.erl`
- `src/async_sf_runtime.erl`
- `src/async_sf_transform.erl`
- `src/monad_frame.erl`

所有 test paths、`openspec/**`、`docs/plan/sf-cc/**`、status 与其他 source modules均不属于本任务。若完成需要修改未列出的 production path，停止并报告精确 symbol/caller 与所需 authority，不自行扩 scope。

## Capability ownership and disposition

| Capability | Disposition | Semantic owner | Required result | Retired symbols/callers |
|---|---|---|---|---|
| external-fun source validation | reuse | `async_sf_cc_macro` | 保留 explicit external `/2|3` gate、AST term与diagnostic location | 无；禁止 runtime `fun_info` source-origin revalidation |
| public facade/result projection | extend | `async_m` | signatures/normalization不变并全部进入common lifecycle | 退役任何绕过outer node的delegation；保留`run_sf_frame/4`为same-version additive facade |
| action records | replace | `async_internal.hrl` | exact ordinary/stable records和direct `parent_ref` | `sf_cc_mref` field与无parent reader |
| backend/classifier | extend | `async_util` | stable_action/ordinary_action/intermediate_node/outer_node/accumulator/other一致分类；reply只认action | `sf_cc_ref` classifier与把node计入`callback_exists/1`的路径 |
| node lifecycle | replace | `async_sf_cc` | constructors、private reverse index、`dispatch_continuation/4`、cleanup、inspection、fail单一owner | CcRef `new_ref/2`/`terminal/1`、root scan/cleanup/dedupe、old recursive summary及callers |
| writer/router/context | extend | `async_t` | all-runner outer registration；actual K/runner classifier；one-level parent materialization；message non-consumption | `run_with_descriptor` CcRef terminal、`new_next/2` callers、root-ref writer logic |
| AccRef vs continuation scope | replace | `async_r_t` | AccRef local保持；current/parent context独立显式传播 | `get_sf_cc_ref/1`、`local_sf_cc_ref/3`及callers |
| frame resolution | replace | `async_sf_runtime` | 每node按owner/Vsn/current manifest验证并运行current MFA；failure只forward parent | `sf_next`/`sf_cc_ref` types、recursive downstream、新旧terminal completion、block_path/async_id lookup |
| frame typeclass | reuse | `monad_frame` | 现有bind/then/default接口返回原Monad | 无第二 sequencing abstraction |
| source lowering | extend | `async_sf_transform` | frozen two-pass、lexical Monad rewrite、minimal Env、generated Monad frame | Pass 2 re-analysis或generated AST re-entry |
| identity/manifest | replace | `async_sf_transform` | direct Block/StepName identity、readable MFA、current-only manifest | hash/hex/AsyncId/block_path/source-index/parent/capture identity与old readers |

## Constraints and invariants

1. 所有 runner invocation先注册outer node；immediate completion同次evaluation消费，suspended action只保存direct parent。ordinary/wait正常public result不变，只有sf-prefixed macros施加external-source restriction。
2. `#callback{}` 和 `#sf_callback{}` 保留完整action lifecycle fields；`parent_ref`不是AccRef、Block identity、resource owner或root Ref。`local_ref/3`只重绑定AccRef。
3. Promise writer仅在actual `K =:= SfContext.runner`时写stable record；否则精确保存actual anonymous `K`与current direct parent。ordinary continuation不得通过fun/map shape猜成frame。
4. frame/ordinary/outer node共用reference key与callbacks dictionary。internal node不是external reply target；presence=pending、absence=consumed，不保留done tombstone。
5. reverse ownership metadata由`async_sf_cc`私有维护，使用non-reply namespaced key与去重稳定child Ref set；registration/take同步更新，不进入execution identity。合法constructor只让新Ref指向已存在parent；self/cycle/depth/orphan/invalid shape按structural failure处理，shared parent允许且只take一次。
6. reply/timeout/DOWN按既有action lifecycle消费ActionMRef并恢复两个scope；message执行现有continuation semantics但不消费ActionMRef或continuation node。
7. ordinary business Outcome总先进入current `cc`/frame。availability catch必须term-exact并位于nearest invocation boundary；进入user callback后的异常原样传播。module `nofile`/missing exported MFA可规范化为unavailable，其他load error原样返回/传播。
8. direct `/3` callback只看到CleanState并必须返回compatible State；`async_r_t`或invalid value在outer node已消费后失败。direct `/2` return忽略；async `/2`只接受并运行`async_r_t`。
9. inspection默认只含request_mref、stable、direct parent_ref、continuation_kind与current frame的module/literal vsn/protocol/block/step_name；privacy blacklist中的任何数据不得进入summary。chain view若保留/提供，必须显式、sanitized、cycle/depth bounded。
10. fail matcher对snapshot按RequestMRef term order preflight；exception/non-boolean在mutation前终止。selected stable按同序commit，ordinary返回`ordinary`且保持pending；已经消费的node不回滚。
11. `sf_do`不拥有final descriptor或runner。nested frames每层是`InnerFrame + OuterRef`，owner modules逐node独立resolve；StoredEnv extra keys允许，required key缺失不执行frame并forward failure。
12. named Block module-wide唯一，StepName在Block内唯一；拼接MFA与user/generated Function/2 collision及atom过长在Pass 1报primary diagnostic，不hash/truncate/fuzzy-match。unnamed locator不成为migration identity。
13. capture只含suffix实际使用且boundary前lexically bound的source variable atom，按atom排序；generator/Input/fresh shadow排除，case/receive/try equality reference保留。CaptureAbi就是name list，不另加hash。
14. local `return/1`/`fail/1`只在当前lexical `sf_do` Monad scope rewrite；remote/outside call不变，non-variable generator mismatch走`monad_fail:fail(monad_badmatch,Monad)`，pure let mismatch保持`badmatch`，final Monad action不消耗name/生成frame/manifest entry。
15. preserve unrelated worktree changes；不保留没有current caller/contract的legacy adapter，不通过catch-all推断capability，不创建parallel lifecycle/identity/diagnostic representation。

## Implementation simplicity

- Standard: audit-implementation-simplicity
- Task-specific requirements: 以规范语义而非旧tests为baseline；`async_sf_cc`是node lifecycle/cleanup/inspection/failure唯一owner，`async_t`只写入与路由，`async_sf_runtime`只解析current frame，`async_sf_transform`只拥有source plan/identity/manifest；复用existing callbacks backend、Monad frame facade与Astranaut traversal；designed diagnostics在transform owner产生raw reason，runtime exception只在term-exact nearest boundary转换；AccRef、ContinuationRef、Context、Env与reverse ownership各有单一显式channel；删除`sf_cc_ref`/`sf_next`/`sf_cc_mref`/hash identity及全部caller，不保留猜测式compatibility；主路径保持preflight→take→direct cleanup→invoke/forward线性，禁止root scan、recursive payload、catch-all和立即二次normalize的private shape。
- Required evidence: owned-path diff与symbol/caller inventory；`sf_cc_ref|sf_next|sf_cc_mref|block_path|async_id|crypto:hash`在new primary path的bounded omission search及每个retained match的current-contract理由；new records/node/context/manifest producer→carrier→consumer map；designed-error/exception-boundary表；reverse-index writer/reader/lifetime/cleanup证据；all public runner→outer node call-site matrix；blocking commands完整输出与clean `git diff --check`。

## Ordered work

1. 在shared header定义ordinary/stable完整records；扩展`async_util`的统一classifier与map/list/dict semantics，使external router只能取action entries，`callback_exists/1`只统计pending actions。
2. 在`async_sf_cc`建立single-shape ContinuationRef constructors、ordinary/outer node executors、private reverse ownership metadata、cycle/depth/orphan guard与统一`dispatch_continuation/4`；先实现node-local preflight/take/direct cleanup，再实现normal/failure forwarding、exact callback mode normalization、current-only inspection与active fail。
3. 在`async_r_t`删除root CcRef local，保留AccRef并显式承载current/parent context；在`async_t`让所有public runners注册outer node，重写actual-K writer、bind_frame direct-parent materialization、reply/message/timeout/DOWN decoder与scope restoration。ordinary downstream需要可寻址时只物化一个ordinary node。
4. 将`async_sf_runtime`收敛为current frame validation/resolution/invocation：验证protocol/shape/owner/literal Vsn/current manifest/MFA/required captures；returned Monad在parent context运行；所有structural failure只dispatch direct parent。删除recursive downstream与terminal CcRef completion。
5. 扩展`async_sf_transform` Pass 1 frozen plan以承载Block/StepName、direct generated name、minimal sorted captures、literal Vsn与current manifest entry；聚合primary diagnostics并error-gate Pass 2。Pass 2只消费plan，生成`'$sf_<block>__<step>'/2` Monad frames和one-shape manifest；保留lexical return/fail/pattern/final-action semantics。
6. 保持`async_sf_cc_macro` AST-preserving gate；更新`async_m` facade与`run_sf_frame/4` compatibility routing；确认wait、ordinary、sf-prefixed immediate/suspended paths均经过outer lifecycle且正常result不变。
7. 全量更新owned callers并删除retired symbols/helpers。执行primary-path omission search，证明没有root/final duplication、recursive continuation、derived identity、ambient continuation state、broad exception inference或并行owner。
8. 运行blocking validation；记录deferred旧shape case的实际签名供`task-test-2`，但不得修改tests或把未知/product failure标成expected transition。

## Validation evidence

handoff 必须包含：每个owned file的changed symbol摘要；R1–R19与sf-do linked requirement到production entry point的map；public runner→outer node→action entry→dispatcher→current executor trace；ordinary/stable/intermediate/outer/accumulator classifier matrix；resource/reverse-index cleanup trace；exception table `call site | expected exception | catching owner | resulting semantics | unexpected propagation | justification`；transform Pass 1/Pass 2与manifest sample；retired symbol omission results；blocking命令exit与`git diff --check`。

## Blocking self-validation

- Commands: `rebar3 compile`; `rebar3 xref`; `rebar3 eunit --module=async_sf_transform_tests`; `git diff --check`

## Deferred downstream validation

- Commands: `rebar3 ct --suite=test/async_sf_cc_SUITE.erl`; `rebar3 ct --suite=test/async_sf_SUITE.erl`; `rebar3 ct --suite=test/async_t_SUITE.erl`; `rebar3 ct --suite=test/async_sf_cc_macro_SUITE.erl`
- Owner: task-test-2
- Expected transitional failures: 仅限existing test assets对`sf_cc_mref`/`sf_cc_ref`/recursive `sf_next`、`[sf_next,sf_next,sf_cc_ref]`、old summary `frames|block_path|async_id`、SHA/hex generated MFA与old manifest `blocks[path].asyncs[AsyncId]`的精确assertion mismatch；ordinary behavior和macro source-gate baseline不得失败，任何crash、timeout、resource leak、wrong status/result或未列出的case均不是accepted transition。
- Closure gate: task-test-2迁移上述stale assertions并以public document-driven linked suites替代后，四个deferred Common Test suite及task-test-2全部blocking commands在merged integrate candidate上exit 0。

## Observable completion

- 所有owned production paths只存在一个ContinuationRef encoding、一个dispatcher与一个classifier authority；old root/recursive/derived identity primary path及callers已退役。
- compile/xref/formatter EUnit/format checks通过；worker evidence可从public runner入口连续追踪到exactly-once current-node execution。
- candidate满足contract内representation、ownership、error/exception、context、compatibility和transform gates，且只剩明确由`task-test-2`拥有的旧assertion transition。
- 没有修改任何test/OpenSpec/workflow/status artifact，也没有stage/commit/merge。

## Stop conditions

- normative inputs、context binding或HEAD/base不再匹配本contract。
- 必须读取/实现`sf-migrate`，或需要定义converter outcome/mapping/downgrade policy。
- 需要修改owned paths之外的production authority，或发现`run_sf_frame/4`存在与本计划compatibility冻结冲突的current normative caller。
- 无法在不创建第二套representation/dispatcher/root index的情况下满足lifecycle，或exact parent/index/cycle语义出现规范冲突。
- blocking command暴露无法在owned paths修复的baseline/environment failure；deferred命令出现未列出的product failure。
- worktree含与owned path重叠且无法安全保留的unrelated change。

## Handoff

向dispatcher返回candidate revision（不自行commit时返回exact worktree diff identity）、blocking command results、deferred transition signatures、required evidence bundle、retained compatibility理由与任何stop condition。明确声明`task-test-2`是唯一downstream closure owner；不得接受计划、record-draft、修改status、stage/commit或启动测试任务。

