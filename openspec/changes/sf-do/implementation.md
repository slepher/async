# sf-do Implementation Contract

## 1. Compatibility Baseline

- ordinary `do` 继续由 ErLando transform 处理；
- legacy `#callback{}` tuple layout、callbacks field 和 router 行为不变；
- `async_r_t:get_state/put_state` 继续隐藏 callbacks store；
- stable path additive，不把 legacy closure 转成 frame。

## 2. Source and Result Type

```erlang
sf_do([Monad || Qualifiers]) -> monad:m(M, A).
sf_do(BlockName, [Monad || Qualifiers]) -> monad:m(M, A).
sf_do(BlockName, StepNames, [Monad || Qualifiers]) -> monad:m(M, A).
```

`sf_do` MUST 返回原 Monad，不得返回 `{sf_program, Frame}` 或 instruction program。

BlockName MUST 是 atom literal。StepNames MUST 是已声明 record name 或完整 atom literal list；
名称按 async-only order 与所有非最终 `bind`/`then` boundaries 一一对应，pure let 和 final action
不消费名称。Monad head MUST 是 atom、variable 或二元 transformer descriptor `{T, Inner}`。
所有形式都统一生成 `monad_frame:*` 调用，再由运行时 typeclass dispatcher 解析 instance；transform
MUST NOT 根据 AST shape 直调 Monad module，也不负责验证 instance export/typeclass metadata。

anonymous BlockName MUST 使用 `'-Function/Arity-fun-N-'`，其中 `N` 是同 source function 内仅对
anonymous `sf_do` 按 lexical pre-order 计算的零基序号。named BlockPath 在 module 内 MUST 唯一；
相同 local name 只可在不同 parent path 下复用。record StepNames MUST 使用 field declaration order
并忽略 default/type；record/list mode 都必须完整覆盖 boundaries，且 StepName 不得重复。

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

## 4. Frame and Dedicated sf_cc Data

持久化 encoding 固定为 tagged map；不接受前一版 prototype tuple 或无 tag map：

```erlang
-type sf_frame() :: #{
    kind := sf_frame,
    protocol := 1,
    block_path := [atom()],
    async_id := binary(),
    mfa := {module(), atom(), 2},
    env := #{atom() => term()}
}.

-type sf_cc() :: #{
    kind := sf_cc,
    protocol := 1,
    mfa := {module(), atom(), 2},
    context := term()
}.

-type sf_continuation() ::
    #{
        kind := sf_next,
        frame := sf_frame(),
        downstream := sf_continuation()
    }
    | sf_cc().
```

generated frame invocation：

```erlang
Module:Function(Input, Env) -> monad:m(M, A).
```

terminal `sf_cc` invocation：

```erlang
Module:Function(Outcome, Context) -> term().
```

`sf_cc` MUST 由用户显式提供 external MFA/context，并由专有 `invoke_sf_cc` interface 调用。
stable path MUST NOT 把它传给 legacy `callback_to_cc`、`run_cc` 或 fun CC API。`sf_cc` 不属于
block manifest 或 migration。

`frame_context()` MUST 恰好提供 `protocol`、`block_path`、`async_id` 和 `env`；runtime 将它与
`frame_mfa()` 合成 `sf_frame()`。callback MUST 用一个 `sf_continuation()` 字段保存 next frame 和
downstream，chain 必须以 `sf_cc()` 终止。

## 5. Compiler Pipeline

transform MUST 使用 Astranaut AST-aware traversal：

1. 第一遍只 collect original `sf_do` occurrences、function owner、lexical nesting、qualifiers、
   bindings 和 source statement table，不修改 AST；
2. 用 `{SourceFunction, LexicalBlockPath, OccurrenceIndex}` 为 occurrence 分配稳定 id，并保存
   original-node digest；
3. 在统一 analysis 阶段计算 per-boundary capture、identity、generated name，并 cross-validate
   block/step/function conflicts；
4. 第二遍按同一 lexical pre-order id 查找冻结的 plan，并校验 original-node digest；
5. 第二遍在 lexical Monad stack 下 rewrite local `return/1`、`fail/1`，在 post-order 中先转换
   nested `sf_do`，再生成 outer `bind_frame`/`then_frame` chain 和 external frame functions；
6. traversal 完成后才插入 generated functions、exports 和 manifest，避免生成物再次进入 source
   collection/rewrite。

不得使用 generic tuple/list walk 作为 compiler AST traversal。不得以 generated AST 重新计算
source AsyncForm identity。第二遍 MUST NOT 重新分析 boundary identity/capture；不含 `sf_do` 的 source
function MAY 跳过第二遍。

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
直接返回，不调用 `bind_frame`/`then_frame`，不消耗 AsyncIndex/StepName，也不生成 frame function
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

stable runner：

```erlang
-spec run_sf_frame(MonadAction, SfCc, Offset, State) -> State.
```

语义：

```text
run MonadAction with dedicated SfCc

bind_frame(Action, NextFrame) under Downstream:
  immediate -> invoke NextFrame(Input), run returned Monad under Downstream
  suspend   -> store NextFrame + Downstream

then_frame(Action, IgnoreInputFrame) under Downstream:
  -> default delegates to bind_frame with the same frame/downstream path
  -> IgnoreInputFrame discards Input before running its remainder

final reply -> invoke_sf_cc(SfCc, Outcome)
```

callback entry MUST 保存一个 `sf_continuation()` chain；每个 `sf_next` 保存 generated frame 和
downstream，chain 以 `sf_cc` 终止。不得保存第二种等价 tuple encoding，也不得由 transform 生成
local bind continuation fun。该约束不授权 `sf_do` 检查 Env/context 中任意 runtime value。
runner/invoker MUST 是 stable path 专有 API，不得调用 legacy CC runner。

## 10. Router Outcomes

- final reply、timeout 和 DOWN MUST 作为 Input 恢复 current frame；
- message MUST NOT consume/advance frame；
- source pattern/Monad 决定 ordinary failure；
- runtime structure corruption、missing manifest/MFA 属于 runtime failure，不伪装成 action
  timeout/down；`sf_do` 不定义对 Env/context 任意 term 的 stable-term validation。

runtime structure corruption、unsupported protocol 或 manifest/MFA validation failure MUST 清理
callback，并恰好一次调用 terminal `sf_cc`，Outcome 为 `{error, {sf_runtime, Reason}}`；不得 invoke
generated frame 或进入 ordinary Monad retry。code-change identity/capture incompatibility 使用第 12 节
的 directional outcome，不包装为 `sf_runtime`。

entry cleanup MUST 复用 existing request lifecycle，保持 correlation key、monitor、timer、request
和 accumulator ownership。

## 11. Identity and Manifest

derived AsyncId：

```erlang
sha256(canonical({sf_async, Protocol, BlockPath, AsyncIndex, AsyncForm})).
```

named AsyncId：

```erlang
sha256(canonical({sf_named_async, Protocol, BlockPath, StepName})).
```

AsyncForm MUST 只包含 pending Action。pattern、suffix、captures 和 runtime Env MUST NOT 参与。

`canonical/1` MUST 删除 source position/annotation、alpha-normalize lexical binder/reference，并把
map 转为 canonical-key 排序 pair list。digest MUST 使用
`crypto:hash(sha256, term_to_binary(CanonicalTerm))`；默认 Protocol 为 `1`，语义级全局变化通过
bump Protocol 表达。generated function atom MUST 是完整 AsyncId 的 lowercase hex，前缀为 `'$sf_'`，
不得截断。

transform MUST export `'$async_sf_manifest'/0`。manifest entry MUST 提供 identity mode、
AsyncIndex/form digest、MFA 和 required capture schema。full source AST、runtime Env、`sf_cc`、
migration selector 和 historical data MUST NOT 写入 current manifest。

## 12. Environment Compatibility

automatic resume requires：

```text
same AsyncId/MFA
and TargetRequiredCaptures subset of StoredEnv
```

extra source Env keys MUST be allowed。缺少 target capture 时 MUST NOT 重放前置 action 或猜测值；
若 `sf-migrate` Env migrator 未在恢复前生成 TargetEnv，upgrade MUST 返回
`{error, {appup, incompatible_frame}}`，downgrade MUST 返回
`{error, {appdown, incompatible_frame}}`。

## 13. Required Diagnostics

- invalid `sf_do` arity/head/block/step names；
- empty block / invalid last qualifier；
- invalid Monad head source form；
- duplicate block path/step name/AsyncId/generated function；
- anonymous block outside source function；
- missing target capture；
- transform ordering/source occurrence mismatch。

## 14. Required Verification

- result remains original Monad and composes with ordinary bind；
- generated functions return Monad rather than `{await|done}` instructions；
- bound qualifiers lower through `bind_frame`；unbound qualifiers lower through `then_frame`，whose
  default delegates to `bind_frame` and whose frame ignores Reply；
- variable/non-variable patterns and generator-variable shadowing match `classic_do` behavior；
- local `return/1`、`fail/1` 的 scope、nested Monad dispatch 和 remote-call behavior 与
  `classic_do` 一致；
- final `return(A)` 和 arbitrary Monad action 直接返回，final generator/match 被拒绝；
- nested named/anonymous `sf_do` suspends and resumes through outer continuation；
- immediate/reply/message/timeout/DOWN preserve ordinary async semantics；
- transform-generated continuation slots contain next/composite frame or dedicated `sf_cc` descriptor，
  not a transform-generated anonymous bind closure；
- stable completion path does not call `callback_to_cc`/legacy `run_cc`；
- current-liveness capture excludes unused bindings and generator pattern variables，and includes the
  lexical Monad value needed for generated dispatch；
- continuation-only changes preserve AsyncId/MFA；
- missing future capture requires exact directional failure；
- ordinary do and legacy callback suites remain unchanged。
