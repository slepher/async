# sf-do Implementation Contract

## 1. Compatibility Baseline

- ordinary `do` 继续由 ErLando transform 处理；
- ordinary callback semantics、callbacks field 和 router behavior 保持兼容；`sf-cc` MAY 在 private
  `#callback{}` 尾部追加 `sf_cc_mref`，新 reader MUST 接受旧三元和新四元 callback tuple；
- protection 以 callback 创建时间区分：升级后 wait/ordinary/sf-cc runners 新建的 action callback
  MUST 填充 CcRef；只有升级前已经 pending 的旧三元 tuple decode 为 `sf_cc_mref = undefined`；
- `async_r_t:get_state/put_state` 继续隐藏 callbacks store；
- stable path additive，不把 legacy closure 转成 frame。

## 2. Source and Result Type

```erlang
sf_do([Monad || Qualifiers]) -> monad:m(M, A).
sf_do(BlockName, [Monad || Qualifiers]) -> monad:m(M, A).
```

`sf_do` MUST 返回原 Monad，不得返回 `{sf_program, Frame}` 或 instruction program。

BlockName MUST 是 atom literal。StepName MUST 作为 standalone literal atom qualifier 内联，并只
能修饰紧随其后的一个非最终 `bind`/`then` boundary。marker MUST 从 executable qualifiers 中删除，
不得作为 Monad action 执行。standalone literal atom qualifier 在没有后继 boundary、修饰 pure let
或 final action、连续 marker 等其他位置 MUST 产生 source error。同一 BlockPath 内 StepName 不得
重复。未被 marker 修饰的非最终 boundary MUST 产生 source-position warning，但允许继续 Pass 2 和
普通执行。Monad head MUST 是 atom、variable 或二元 transformer descriptor `{T, Inner}`。
所有形式都统一生成 `monad_frame:*` 调用，再由运行时 typeclass dispatcher 解析 instance；transform
MUST NOT 根据 AST shape 直调 Monad module，也不负责验证 instance export/typeclass metadata。

`sf_do/1` 的 anonymous BlockPath component MUST 使用 `'-Function/Arity-fun-N-'`，其中 `N` 是同
source function 内仅对 anonymous `sf_do` 按 lexical pre-order 计算的零基序号。`sf_do/2` 的 named
BlockPath 在 module 内 MUST 唯一；相同 local name 只可在不同 parent path 下复用。

## 3. monad_frame Contract

逻辑类型：

```erlang
-callback bind_frame(
    monad:m(M, A),
    frame_mfa(),
    frame_context(),
    M
) -> monad:m(M, B).

-callback then_frame(
    monad:m(M, A),
    frame_mfa(),
    frame_context(),
    M
) -> monad:m(M, B).
```

`frame_mfa()` 固定为 `{module(), atom(), 2}`。固定 instance facade
可以导出 `bind_frame/3`、`then_frame/3`，但 transform 只生成通用 dispatcher call。
`monad_frame` MUST 提供：

```erlang
default_then_frame(Action, IgnoreInputMFA, Context, MonadFrame) ->
    bind_frame(Action, IgnoreInputMFA, Context, MonadFrame).
```

这里 Reply 的丢弃由 `IgnoreInputFrame` 的 generated function 完成；default 本身直接复用
`bind_frame` 的 dispatch/suspend path。这对应 `classic_do` 把无绑定 Action 编译为 `>>=` 加
`fun(_) -> Remainder end`，以及 Monad `>>` default 委托 `>>=`。

该 capability：

- MUST 保持 Monad 类型；
- MUST 具有 ordinary bind 的 sequencing/outer-continuation 语义；
- `bind_frame` MUST 把 Input 交给 data frame continuation；
- `then_frame` MUST 丢弃 Input，并具有 `default_then_frame` 的可观察语义；
- 两者 MUST 只接受 data frame continuation；
- MUST NOT fallback 到 anonymous fun bind；
- MUST 在 action 执行时若 immediate completion 则立即 invoke frame，并在同一 downstream 下运行其返回 Monad；
- MUST 在 action suspension 时把 frame 与 downstream stable continuation 持久化。

## 4. Frame Data and sf_cc_ref Terminal

持久化 encoding 固定为 tagged map；不接受前一版 prototype tuple 或无 tag map：

```erlang
-type sf_frame() :: #{
    kind := sf_frame,
    protocol := 1,
    module := module(),
    vsn := term() | undefined,
    block_path := [atom()],
    step_name := atom() | undefined,
    async_id := binary(),
    mfa := {module(), atom(), 2},
    env := #{atom() => term()}
}.

-type sf_continuation() ::
    #{
        kind := sf_next,
        frame := sf_frame(),
        downstream := sf_continuation()
    }
    | #{kind := sf_cc_ref, protocol := 1, mref := reference()}.
```

generated frame invocation：

```erlang
Module:Function(Input, Env) -> monad:m(M, A).
```

`module` MUST 与 `mfa` owner module 相等。`vsn` MUST 是 producer 取得的 explicit module `-vsn`
literal term；没有可用显式值时为 `undefined`，不得假定 atom/string/semver，也不得保存 OTP attribute
container 的外层 list。Vsn 不进入 AsyncId。

`frame_context()` MUST 提供 `protocol`、`module`、`vsn`、`block_path`、`step_name`、`async_id` 和
`env`；runtime 将它与 `frame_mfa()` 合成 `sf_frame()`。callback 的 `cc` MUST 保存 next frame 与
downstream chain，并以 `sf_cc_ref` 终止。完整 sf_cc descriptor、external callback 和 Context MUST
只由 sf-cc 的 CcRef entry 保存，不得嵌入 chain。保存该 chain 的 callback.sf_cc_mref MUST 与
terminal mref 相等。

## 5. Compiler Pipeline

transform MUST 使用 Astranaut AST-aware traversal：

1. 第一遍只 collect original `sf_do` occurrences、function owner、lexical nesting、qualifiers、
   bindings 和 source statement table，不修改 AST；
2. 用 `{SourceFunction, LexicalBlockPath, OccurrenceIndex}` 为 occurrence 分配稳定 id，并保存
   original-node digest；
3. 在统一 analysis 阶段配对 inline name markers，并计算 per-boundary capture、named identity 或
   unnamed local locator、generated name，并
   cross-validate block/step/function conflicts；unnamed boundary 记录 warning；
4. 第一遍的 expected source errors MUST 通过 Astranaut return value 累计；第一遍完成后若有任何
   error，MUST 立即调用 `astranaut_return:to_compiler/1` 返回，且 MUST NOT 初始化或运行第二遍；
5. 无错误时，第二遍按同一 lexical pre-order id 查找冻结的 plan，并校验 original-node digest；
6. 第二遍在 lexical Monad stack 下 rewrite local `return/1`、`fail/1`，在 post-order 中先转换
   nested `sf_do`，再生成 outer `bind_frame`/`then_frame` chain 和 external frame functions；
7. traversal 完成后才插入 generated functions、exports 和 manifest，避免生成物再次进入 source
   collection/rewrite。

不得使用 generic tuple/list walk 作为 compiler AST traversal。不得以 generated AST 重新计算
identity。expected validation failure MUST NOT 使用 `throw`/`error` 逃离 traversal；unexpected
internal fault 可以作为单独 compiler internal error 报告。第二遍 MUST NOT 重新执行 source-form、
name-marker、boundary identity 或 capture validation；不含 `sf_do` 的 source function MAY 跳过第二遍。

## 6. Qualifier Lowering

假设 remainder function 为 `FrameN`：

```text
Pattern <- Action
    -> monad_frame:bind_frame(Action, FrameNMFA, Context, Monad)

Action
    -> monad_frame:then_frame(Action, IgnoreInputFrameNMFA, Context, Monad)

Pattern = Expression
    -> ordinary Erlang match inside generated frame body

final Action
    -> Action
```

generated frame MUST 直接返回 remainder Monad。`bind_frame` frame MUST 用 Input 执行 source
pattern；`then_frame` frame MUST 接收但忽略 Input。除 continuation representation 和调用的
sequencing primitive 外，两者分别保持 `classic_do` generator 和无绑定 Action 的 lowering 语义。

final qualifier MUST 是 Monad action，可以是 local `return(A)` 或其他 do Action。final `<-`
generator 和 final pure match MUST 在 transform 阶段报错。final action MUST 与 `classic_do` 一样
直接返回，不调用 `bind_frame`/`then_frame`，不消耗 StepName，也不生成 frame function
或 manifest entry。若 final action 挂起，runtime MUST 保存进入它时已有的 downstream，不得添加
synthetic sf-do frame。

与 `do` 一致，`sf_do` lexical subtree 内的 local `return/1`、`fail/1` MUST 分别 rewrite 为
`monad:return(Value, Monad)`、`monad_fail:fail(Reason, Monad)`；两者均由通用 dispatcher 处理，
不得按 tuple AST shape 直调 module。remote call 和 block
外同名 local call MUST 保持不变。nested `sf_do` MUST 使用 lexical Monad stack，使 inner call 使用
inner Monad，离开 inner block 后恢复 outer Monad。
只有实际使用 local `fail/1` 或 non-variable generator pattern 的 block 才依赖 `monad_fail`；
transform MUST NOT 预检该 capability。

非变量 generator pattern MUST 在 external frame body 中用 `case Input of` 实现：

```erlang
case Input of
    Pattern -> RemainderMonad;
    _ -> monad_fail:fail(monad_badmatch, Monad)
end.
```

generator pattern variables MUST 与 `classic_do` anonymous fun clause 一样成为新 bindings；
enclosing scope 的同名 variable MUST 被 shadow，不得预先从 Env restore 成 equality constraint。
pure let mismatch 仍是 Erlang `badmatch`。

## 7. Capture Analysis

```text
Required(Frame_i) =
    FreeVariables(Continuation_i)
    ∩ BoundBeforeBoundary_i
```

analyzer MUST scope-aware，并至少正确处理 clause/fun/named_fun/case/try/comprehension、guards、
generator patterns 和 sequential bindings。

`fun`/named-fun clause parameters MUST 创建 fresh bindings；`case`、`receive`、`try` clause
pattern 中引用已在外层绑定的 variable MUST 作为 equality reference，并在 boundary 后使用时进入
free-variable capture。collect、capture 与其他 lexical analyses MUST 共享同一 binding classifier，
不得对 clause pattern 各自实现不一致的 binding 规则。

frame Input 和当前 generator pattern 新 bindings MUST NOT 进入 Env。Monad dispatch 所需的 lexical
Monad value以及 remainder 实际使用的 pre-boundary bindings MUST 进入 Env；未被当前 continuation
使用的 binding MUST NOT 默认捕获。

Env key MUST 直接使用 source variable atom。analyzer 仍必须用 scope-aware binder analysis 计算
free-variable capture set；generator pattern variables、shadowed outer bindings 和 Input 不得错误进入
集合。CaptureLayout MUST 按 atom 排序，target required set 按 map key membership 检查。

变量重命名导致 target key 缺失，同名变量跨版本改变语义则仍命中旧 key；`sf_do` MUST NOT 为这些
source edits 推断 binder equivalence。它们由 `sf-migrate` directional Env migrator 显式生成
TargetEnv。CaptureAbi MUST 直接使用排序后的 capture-name atom list，不得另做 SHA/digest。
transform generated Input/Env/temporary variables MUST hygienic，不得与 source variables 冲突。

Env capture value SHOULD NOT 是 anonymous/local fun，但 transform/runtime MUST NOT 为此递归扫描或
拒绝任意 value。local fun 不会在 upgrade 时自动从 State 中消失或更新；若 defining old code 被
purge，它可能不可调用。需要保留 fun-valued capture 的应用 SHOULD 通过 `sf-migrate` Env migrator
在 target code 中重新生成，或改存 external MFA/data descriptor。自动检测或强制声明属于 optional
follow-up，不是当前 sf-do implementation gate。

## 8. Nested Composition

第二遍 rewrite MUST 支持 nested `sf_do`。inner expression 仍是 `monad:m(M,A)`，可作为 outer
`bind_frame` Action。runtime MUST 保存 inner next frame 和 outer downstream stable frame，使 inner
完成值进入 outer continuation。

nested block identity MUST 使用 original lexical BlockPath；generated function owner 不得进入
child identity。

## 9. Runtime Execution

所有升级后 public runners MUST 委托同一个 CcRef execution core。`sf-cc` 定义
`run_sf_cc/3`、`run_with_sf_cc/5` 和 `exec_sf_cc/5` 的 explicit external-fun contract；现有
`wait/1`、`wait_t/2`、`run_cc/2`、`run_with_cc/4`、`exec_cc/4` 和 `run/4` 保持 public contract，
但作为 compatibility facades 创建 CcRef 并使用 library wait completion或 legacy final adapter。
`sf-do` MUST NOT 另行冻结接受完整 SfCc descriptor 的 public runner；现有 `run_sf_frame/4` 若在过渡
实现中保留，只能是待移除/适配的 internal compatibility path。

语义：

```text
run MonadAction under sf-cc scope carrying CcRef

bind_frame(Action, NextFrame) under Downstream:
  immediate -> invoke NextFrame(Input), run returned Monad under Downstream
  suspend   -> store NextFrame + Downstream in callback.cc
               and CcRef in callback.sf_cc_mref

then_frame(Action, IgnoreInputFrame) under Downstream:
  -> default delegates to bind_frame with the same frame/downstream path
  -> IgnoreInputFrame discards Input before running its remainder

terminal sf_cc_ref -> complete_sf_cc(CcRef, Outcome)
```

callback entry MUST 保存一个 `sf_continuation()` chain；每个 `sf_next` 保存 generated frame 和
downstream，chain 以 `sf_cc_ref` 终止。不得保存完整 sf_cc 的第二 owner，也不得由 transform 生成
local bind continuation fun。该约束不授权 `sf_do` 检查 Env/Action 中任意 runtime value。

reply executor MUST 从 decoded callback 的 `acc_ref` 和 `sf_cc_mref` 重建独立 scopes。resumed frame
产生新的 action callback 时 MUST 继承同一 CcRef，即使 parallel/local accumulator 改变了 AccRef。
一个 Promise 的 nested/parallel implementation 可以物化多个携带同一 CcRef 的内部 action callbacks，
但它们仍有独立 RequestMRef/lifecycle；共享 CcRef 只表达单个最终 Outcome 地址，不建立多 Promise
operation group，也不产生 per-entry final callback。

## 10. Router Outcomes

- final reply、timeout 和 DOWN MUST 作为 Input 恢复 current frame；
- message MUST NOT consume/advance frame；
- source pattern/Monad 决定 ordinary failure；
- runtime structure corruption、missing manifest/MFA 属于 runtime failure，不伪装成 action
  timeout/down；`sf_do` 不定义对 Env/context 任意 term 的 stable-term validation。

runtime structure corruption、unsupported protocol 或 manifest/MFA validation failure MUST NOT invoke
generated frame。executor MUST 直接从 decoded `callback.sf_cc_mref` 调用 sf-cc unified completion，
Outcome 为 `{error, {sf_runtime, Reason}}`；不得通过递归解析损坏 chain 寻找 final callback，也不得进入
ordinary Monad retry。migration mapping/Env/Input failure 的 Outcome 由 sf-migrate 定义，但 MUST 使用
同一个 CcRef completion primitive。

cleanup ordering、CcRef take、关联 action callbacks/timer/monitor/accumulator 清理、exactly-once 和
CleanState callback invocation 完全由 sf-cc contract 定义。sf-do 不得在调用 completion 前自行调用
用户 final callback，也不得在 completion failure 后恢复 CcRef。

### Pre-upgrade unprotected callback

shared callback decoder MUST 把 `{callback, Cc, AccRef}` 读为 `sf_cc_mref = undefined`。router MUST
尝试保持旧 continuation behavior；但当 invocation classifier 能确认 anonymous fun 因所属旧代码已
purge 而不可调用时，MUST：

1. 删除该 RequestMRef callback；
2. 清理仍可定位且确认归属该 request 的 monitor/timer/accumulator lifecycle；
3. 不构造 CcRef、不调用 final CC、不 retry；
4. 返回 `{legacy_outcome, Outcome, CleanState}`，其中 Outcome 是原本将传给旧 continuation 的
   reply/timeout/DOWN/message action Outcome。

`async_m:handle_info/3` return contract 因此是：

```erlang
unhandled
| State
| {legacy_outcome, Outcome, CleanState}.
```

该 tagged return 不是 Promise final completion，只把无法继续解释的原 action Outcome 交给当前 receive-loop
caller。classifier MUST NOT 把已经成功进入 legacy callback 后抛出的普通 user exception、business
failure 或任意 nested `undef` 一律吞掉；unavailable-code detection MUST 结合 fun identity/stack target，
并在 supported OTP versions 上验证。message path 一旦确认 continuation unavailable，也 MUST 删除它，
防止后续 message/reply 重复触发。

## 11. Identity and Manifest

有 inline StepName 的非最终 `bind`/`then` boundary 的 migration AsyncId 为：

```erlang
crypto:hash(sha256,
    term_to_binary({sf_named_async, Protocol, BlockPath, StepName})).
```

AsyncId 只是 protocol、BlockPath 和 StepName 的固定长度编码。source-order index、Action AST、
annotation、binder names、map ordering、pattern、suffix、captures 和 runtime Env MUST NOT 参与，
transform MUST NOT 为 identity 维护 Action canonicalizer。默认 Protocol 为 `1`，语义级全局变化通过
bump Protocol 表达。generated function atom MUST 是完整 AsyncId 的 lowercase hex，前缀为
`'$sf_'`，不得截断。

未被 name marker 修饰的 boundary MUST 生成 `unnamed_sf_action` warning，并获得仅用于普通执行/
same-version resume 的 module-local frame locator 与 external MFA。其 manifest entry MUST 使用
`step_name => undefined`。migration resolver MUST 在调用该 MFA 前拒绝把 unnamed locator 当作可迁移
identity；local locator、source index 和 Action AST MUST NOT 用于寻找 migration target。最终 migration
failure/abort Outcome 属于 sf-migrate，并通过 sf-cc CcRef completion 提交。

transform MUST export `'$async_sf_manifest'/0`。manifest MUST 按 BlockPath/AsyncId 提供
owner Module、module Vsn literal term或 `undefined`、`StepName | undefined`、MFA
和 required capture schema。AsyncForm digest、full source AST、runtime Env、sf-cc descriptor、migration
selector 和 historical data MUST NOT 写入 current manifest。

## 12. Frame Resolution and Environment Compatibility

在调用每个 stored generated MFA 前，runtime MUST 按该 frame 的 owner Module/Vsn 进入 resolution
boundary。same-version direct resume 至少 requires：

```text
StepName is present
and same BlockPath/StepName and their generated AsyncId/MFA
and TargetRequiredCaptures subset of StoredEnv
```

extra source Env keys MUST be allowed。缺少 target capture 时 MUST NOT 重放前置 action 或猜测值；
resolver MAY 使用 sf-migrate 提供的 optional Env converter 生成 TargetEnv，并 MAY 在 reply 到达后使用
optional Input converter 生成 TargetInput。没有 converter 时分别复用 StoredEnv 和原 Input，再按 target
manifest 验证。sf-do MUST NOT 定义 converter return shape、version-chain composition 或 downgrade policy。

unnamed frame 在普通执行和 same-version resume 中 MAY 使用其 local locator；进入 appup/appdown
migration 时 MUST 在 target invocation 前 fail closed。

historical old block/manifest MUST NOT 是 upgrade runtime 的必要输入。actual stored frame 提供 source
endpoint；current owner manifest 提供 target endpoint和 required captures。historical metadata MAY 由
sf-migrate 用于 compiler diagnostics 或 strict downgrade，但不改变 sf-do frame generation contract。

owner module 独立升级不会触发 holder process 的 `code_change/3`。因此 eager migration MAY 提前处理
callbacks，但 correctness MUST 由 reply-time lazy resolution 保证。nested chain 每个 frame 按自己的
owner module 独立 resolve，尚未执行的其他-module nodes MUST bit-for-bit 保留。

## 13. Required Diagnostics

- reachable source errors：invalid arity、non-literal BlockName、non-comprehension argument、invalid Monad
  head、invalid marker placement、duplicate StepName、invalid last qualifier、duplicate BlockPath，以及与
  用户已有 `Function/2` 的 generated-function conflict；
- 每个 unnamed non-final action 的 source-position warning；
- defensive AST error：empty comprehension；它保护 earlier-transform 在合法 function form 内提供的
  malformed AST，不宣称是普通 Erlang source 可输入形式。function 外 application 不是合法 compiler
  form，不定义不可达的领域 reason；
- internal errors：transform ordering/source occurrence mismatch，只携带 occurrence identity 或有界
  `{owner, occurrence, block_depth, plan_count, consumed_count, unconsumed_count}` 摘要；
- missing target capture 属于 sf-migrate/runtime resolver，不是 sf-do source-transform diagnostic；
  AsyncId collision 与其一一派生的 generated-function collision 是同一检查，不重复报告。

Pass 1 MUST 累积可独立报告的 source errors/warnings；存在任一 error 时 MUST 通过
`astranaut_return:to_compiler/1` 返回并证明 Pass 2 未运行。只有 warnings 时 MUST 继续 Pass 2，
并把 warnings 与 transformed forms 一起返回 compiler。

`async_sf_transform:format_error/1` MUST 只包含 transform-owned reason 的精确 clauses，不得包含
generic catch-all 或调用 Astranaut shared dispatcher。领域消息 MUST 说明规则和允许形式，但 MUST NOT
重复 compiler 已展示的 file、line、column、source excerpt 或 offending AST。正常 compiler 路径的
未知 reason fallback MUST 由 `astranaut_lib` 统一拥有；直接调用领域 formatter 的未知 reason MUST
保持 `function_clause`。

duplicate StepName/BlockPath diagnostic 产生后，MUST 跳过该 invalid plan 的下游 generated-name collision
报告。同一个 trailing marker 已报告 dangling marker 时，MUST NOT 再为它使前一个 generator/match 成为
最后 executable qualifier 而报告第二个 invalid-last diagnostic。不同 occurrence 的独立 source errors
仍 MUST 按 source order 累计。

## 14. Required Verification

- result remains original Monad and composes with ordinary bind；
- generated functions return Monad rather than `{await|done}` instructions；
- bound qualifiers lower through `bind_frame`；unbound qualifiers lower through `then_frame`，whose
  default delegates to `bind_frame` and whose frame ignores Reply；
- variable/non-variable patterns and generator-variable shadowing match `classic_do` behavior；
- local `return/1`、`fail/1` 的 scope、nested Monad dispatch 和 remote-call behavior 与
  `classic_do` 一致；
- final `return(A)` 和 arbitrary Monad action 直接返回，final generator/match 被拒绝；
- nested explicit/anonymous-path `sf_do` suspends and resumes through outer continuation；
- immediate/reply/message/timeout/DOWN preserve ordinary async semantics；
- transform-generated continuation slots contain next/composite frame or `sf_cc_ref` terminal，not a
  transform-generated anonymous bind closure or full sf_cc descriptor；
- suspended stable callback keeps continuation in `cc` and the same terminal CcRef in `sf_cc_mref`；
- structural failure reaches sf-cc completion through the callback field even when the frame chain is corrupt；
- stable completion path uses sf-cc runners/primitive and does not call `callback_to_cc`/legacy `run_cc`；
- current-liveness capture excludes unused bindings and generator pattern variables，and includes the
  lexical Monad value needed for generated dispatch；
- pending Action、continuation、source index 或 binder rename changes preserve named AsyncId/MFA；
- inline atom markers are removed from execution；invalid marker placement errors；unnamed boundaries
  warn、execute normally and fail appup/appdown migration before target invocation；
- `case`/`receive`/`try` outer-variable equality patterns capture the outer binding，while
  `fun`/named-fun parameters remain fresh shadowing bindings；
- multiple invalid `sf_do` occurrences are accumulated in Pass 1，and Pass 2 is not entered；
- every emitted transform reason directly matches a strict domain formatter clause；unknown reasons only
  fallback through the Astranaut adapter；
- formatter exact text、unknown-reason behavior、adapter fallback 与 handcrafted AST validation 由 EUnit
  验证；真实 compiler diagnostics、错误累计和生成代码行为由 Common Test 验证；
- compiler diagnostics retain exact line/column ownership while domain messages do not duplicate source text；
- duplicate StepName/BlockPath and trailing-marker errors do not emit derived generated-name/invalid-last noise；
- missing future capture prevents target invocation and terminal-completes through CcRef；
- frame and manifest expose owner Module and literal module Vsn/`undefined` without adding Vsn to AsyncId；
- a frame owned by an independently upgraded module is lazy-resolved before MFA invocation；
- nested multi-module frames resolve one node at a time and preserve other owners' nodes；
- upgrade resolution can use the actual stored frame without historical source block metadata；
- optional Env/Input conversion and downgrade policy are integration-tested with sf-migrate rather than
  redefined by sf-do；
- wait/ordinary/sf-cc runners created after upgrade all allocate CcRef without changing ordinary normal results；
- old three-tuple callback remains readable as unprotected；unavailable old fun returns tagged legacy outcome
  plus CleanState without crashing, retrying or fabricating final completion；
- ordinary do and legacy callback suites remain unchanged。
