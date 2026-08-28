# sf-do Design

## Context

普通 `do` 的 bind continuation 是 anonymous fun。它适合进程内短期执行，但不适合作为
hot-code change 期间的持久 continuation。`sf_do` 使用 external MFA + Env 表示同一
continuation，同时保持原 Monad 类型和普通 bind 组合律。

本设计刻意不引入 `{sf_program, Frame}`、`{await, ...}`、`{done, ...}` 或独立 frame VM。
hot-code upgrade、purge 与 local fun 延迟失效带来的设计动机见 [Why sf-do Exists](rationale.md)。

## Goals

- `sf_do` expression 的类型与其 Monad action 类型相同。
- `bind_frame`/`then_frame` 是 continuation 仅允许 frame 的 sequencing primitives，不是 program
  interpreter。
- transform 不为用户 block remainder 生成 bind continuation anonymous fun；它生成 external
  frame MFA + Env，并通过 `bind_frame`/`then_frame` 交给 Monad。
- stable callback的continuation portion只保存current generated frame与direct `parent_ref`，record还完整
  保存accumulator和timer等action lifecycle metadata；outer frame和final callback
  分别存在于callbacks store的其他continuation nodes，而不是transform生成的remainder closure。
- nested `sf_do` 使用普通 Monad composition。
- identity 描述 source 显式命名的恢复步骤，不描述 Action AST 或其后的完整 continuation。
- Env 是当前 continuation 的最小 lexical environment，不预测未来版本。

## Non-Goals

- 不从 ordinary fun 反向提取 frame。
- 不保存或解释整个 block 的 instruction list。
- 不自动重放 pending boundary 之前的 action 来重建缺失变量。
- 不让 ordinary `do`、ordinary bind 或 ordinary callback 自动获得 migration 保证。
- 不在 `sf_do` 中猜测 future Env schema；变量改名或同名变量含义变化由 `sf-migrate` Env migrator
  在恢复前显式转换 Env。
- 不递归检查或拒绝 Action、Env、capture value、frame context 或用户 final Context 中的任意 runtime
  term；这些值是否包含 fun 不属于 `sf_do` source transform 的权限边界。

## Source Syntax

公开语法：

```erlang
sf_do([Monad || Qualifiers])
sf_do(BlockName, [Monad || Qualifiers])
```

最后一个 argument 是 single-head list comprehension。以下不是公开语法：

```erlang
sf_do([BlockName, StepNames, Monad || Qualifiers]).
sf_do(BlockName, StepNames, [Monad || Qualifiers]).
```

anonymous `sf_do/1` 在 enclosing source function 内按 lexical pre-order获得仅供普通执行和同版本
resume 的 module-local locator；named block不消耗该序号。BlockName 必须是 atom literal。named Block
在 module 内全局唯一，不能因 lexical parent不同而复用。把 named block 提取到其他函数、返回或传递其
Promise，不改变由显式 BlockName 定义的 identity。

StepName 直接写成 standalone literal atom qualifier，并修饰紧随其后的一个非最终 continuation
boundary：

```erlang
sf_do(flow, [async_m
 || load_user,
    User <- fetch_user(),
    save_result,
    persist(User),
    return(User)
]).
```

`load_user`、`save_result` 是 compile-time metadata，不作为 Monad action 执行。standalone literal
atom qualifier 不存在普通-action 语义：它若没有紧邻一个非最终 `bind`/`then` action，或出现在
pure let/final action/连续 marker 等其他位置，必须报 source error。同一 Block 内 StepName
不得重复。未被 marker 修饰的非最终 boundary 仍可编译和普通执行，但必须产生 warning，并标记为
non-migratable。pure let 和 final action 不形成当前 block frame，不需要 StepName。

## 与 classic_do 的核心对照

`sf_do` 以 ErLando `do` 的核心 source semantics 为 baseline。没有在下表列为差异的 qualifier、
pattern、local-call 或 final-expression 行为，均不得由 `sf_do` 另行定义。

| 方面 | `classic_do` | `sf_do` |
| --- | --- | --- |
| public source | `do([Monad || Qualifiers])` | `sf_do/1,2`；StepName 作为 inline atom marker，二参数形式增加 BlockName |
| result | final expression 的原 Monad | 相同；不引入 `sf_program` |
| `Pattern <- Action` | `>>=(Action, FunContinuation)` | `monad_frame:bind_frame(Action, FrameMFA, Context, Monad)` |
| 无绑定 `Action` | `>>=(Action, fun(_) -> Remainder end)` | `monad_frame:then_frame(Action, IgnoreInputMFA, Context, Monad)` |
| discard sequencing/default | `classic_do` 直接使用 `>>=` + `_` continuation；Monad `>>` default 也委托 `>>=` | transform 调用 `then_frame`；其 default 委托 `bind_frame` + ignore-input frame |
| `Pattern = Expression` | 普通 Erlang match | 相同；只改变它位于 call site 或 generated frame body 的位置 |
| final qualifier | 必须是普通 expression，并直接返回 | 相同；不生成 frame、StepName 或 manifest entry |
| generator mismatch | variable pattern 单 clause；non-variable fallback 到 `fail(monad_badmatch)` | 相同；frame 恢复 Env 后执行同一 pattern/fallback |
| local `return/1`、`fail/1` | lexical Monad stack；nested block 使用 inner Monad | 相同；分别统一生成 `monad:return/2`、`monad_fail:fail/2`，remote call 不 rewrite |
| lexical scope/evaluation | zero-argument fun 隔离 block bindings；qualifiers 按 source order、一次求值 | 保持相同可观察语义；call-site lets/generated frames 不得泄漏 binding 或重复求值 |
| continuation representation | anonymous fun 和隐式 closure captures | transform 生成 external MFA + 显式 Env frame，不生成 remainder closure |
| suspension persistence | `classic_do` 本身不提供跨版本保证 | stable runner 持久化 current frame + direct `parent_ref` |
| identity/naming | 无 stable boundary identity 或 manifest | named boundary 使用 Block/StepName；unnamed boundary warning 且不可迁移 |
| compiler pipeline | 单次 Astranaut traversal，post-order lowering | 两遍 Astranaut traversal，先冻结 source facts，再按 plan rewrite |
| completion interface | 不定义 async stable completion | 依赖 sf-cc；统一 ContinuationRef 链到达 outer callback node |

不同 Monad 的 nested lexical block 与同一 Monad 的 direct composition 是两个问题：和
`classic_do` 一样，进入 nested block 时 local `return/1`、`fail/1` 使用 inner Monad；只有 inner
Monad value 被直接交给 outer sequencing primitive 时，才由该 primitive 的普通 Monad 类型约束决定
能否组合。`sf_do` 不额外定义跨 Monad lift。

`classic_do` 中依据 tuple AST 直接选择 leading module 的额外分支属于历史实现细节，不进入本表，
也不是 `sf_do` 合同。`sf_do` 对 atom、variable 和二元 transformer descriptor `{T, Inner}` 均统一
调用 `monad_frame` dispatcher；dispatcher 再按运行时 Monad value 解析 instance。transform 不做
instance export/typeclass metadata 检查，缺少 capability 使用 dispatcher 的标准失败行为。

## Core Semantic Contract

普通 bind：

```text
bind(Action, FunContinuation) -> Monad
```

stable bind：

```text
monad_frame:bind_frame(Action, NextMFA, FrameContext, Monad) -> SameMonad
monad_frame:then_frame(Action, IgnoreInputMFA, FrameContext, Monad) -> SameMonad
monad_frame:default_then_frame(Action, IgnoreInputMFA, FrameContext, Monad)
    = monad_frame:bind_frame(Action, IgnoreInputMFA, FrameContext, Monad)
```

这与 `classic_do` 的两个 lowering 对应：`Pattern <- Action` 使用 bind continuation；无绑定
`Action` 使用丢弃 Reply 的 continuation。`then_frame` 的 default 不改变 suspend/runtime 协议，
只把已声明为 ignore-input 的 frame 交给 `bind_frame`。Monad instance MAY override
`then_frame`，但结果类型和 sequencing 语义必须与该 default 相同。

`NextMFA` 固定为 `{Module, Function, 2}`。`FrameContext` 固定为 map：

```erlang
#{
    protocol := 1,
    block := atom() | undefined,
    step_name := atom() | undefined,
    env := #{atom() => term()}
}
```

两者合成可持久化 frame；generated function 的逻辑调用协议：

```text
invoke(Frame, Input) -> Monad
```

因此：

```erlang
sf_do(flow, [
    async_m
 || request1_step,
    Reply1 <- request1(),
    request2_step,
    Reply2 <- request2(Reply1),
    async_m:pure_return({Reply1, Reply2})
]).
```

等价于：

```erlang
monad_frame:bind_frame(request1(), Frame1MFA, Context1, async_m).

frame1(Reply1, Env1) ->
    monad_frame:bind_frame(request2(Reply1), Frame2MFA, Context2, async_m).

frame2(Reply2, Env2) ->
    Reply1 = maps:get('Reply1', Env2),
    async_m:pure_return({Reply1, Reply2}).
```

generated frame function 直接返回 Monad。`bind_frame` 对应的 function 消费 Input pattern；
`then_frame` 对应的 function 接收但忽略 Input。runtime 不读取 step instruction tag。

## sf-cc Dependency and Completion Reference

一个 Monad value 只有在 runner 提供最终 continuation 后才执行，但 public runner、external callback
source check、Context、callback arity/return contract、ContinuationRef store 和 dispatch transaction 全部由独立
`sf-cc` capability 定义。sf-do 不拥有 `run_sf_frame/4` 形式的 public final API，也不组装或调用完整
sf_cc descriptor。

generated frame 与 final completion 是不同接口：前者 `(Input, Env) -> Monad`，后者由 sf-cc 消费最终
Outcome。两者只通过 linked ContinuationRef nodes 建立联系。

持久化数据默认固定为 tagged map，不保留 tuple/map 双格式：

```erlang
SfFrame = #{
    kind := sf_frame,
    protocol := 1,
    module := Module,
    vsn := Vsn | undefined,
    block := Block | undefined,
    step_name := StepName | undefined,
    mfa := {Module, Function, 2},
    env := Env
}.

SfContinuation = #{frame := SfFrame, parent_ref := ContinuationRef}.

ContinuationRef = reference().
```

stable callback 使用一个单层 `SfContinuation`。收到 Input 后 invoke `frame`，再把返回 Monad 运行在
`parent_ref` 指向的直接外层 node 下；它不是 action instruction 或 frame VM。callback 不保存递归
downstream，也不另存 root/final reference。

`module` 必须与 frame MFA owner 一致。`vsn` 是 producer 捕获的 module `-vsn` literal term；没有可用
显式 Vsn 时为 `undefined`。它不得被强制转换成 atom/string/semver，也不进入 boundary identity。

`bind_frame` 必须保留直接 downstream reference：

```text
run bind_frame(Action, NextFrame) with ParentNode
  -> materialize ParentRef when ParentNode is not already addressable
  -> run Action with data continuation {NextFrame, ParentRef}

reply
  -> invoke NextFrame(reply) to obtain NextMonad
  -> run NextMonad with ParentRef
```

`ParentRef` 可以指向 outer generated frame、ordinary continuation 或最外层 callback descriptor。
callback 只保存 NextFrame 与该直接 Ref；不得复制 ParentNode 内容、递归父链或 root Ref，也不得把
ordinary fun 嵌入 stable frame data。

`async_t` 通过显式 `(K, SfContext)` 传递 structured continuation context，不得使用进程字典。ordinary
bind 继续创建 anonymous `K`并透传 direct parent scope；真正挂起的 Promise writer将该 `K` 与
`SfContext.parent_ref` 写入 ordinary `#callback{}`。`bind_frame` 收到尚未可寻址的 frame 或 ordinary
anonymous downstream 时，runtime MUST 在 callbacks store 中物化一个 continuation node，并把其 Ref
作为新 frame 的 `parent_ref`。bind 不遍历或拆包 Promise；只有执行到 suspension writer时才物化 action entry。

用户 final callback 不属于 do block transform：

- 不进入 named boundary identity；
- 不进入 block manifest；
- 不进入任何 external migration descriptor；
- Context 不由 lexical Env 推导或迁移；
- 完整 callback descriptor 只存在于 sf-cc outer node；
- 用户要迁移 completion 逻辑时，应把逻辑放进 `sf_do`，最后接 sf-cc external callback。

### Universal ContinuationRef adoption

ContinuationRef 是 runner engine 的统一 lifecycle primitive，不是只有显式 sf-cc API 才创建的 opt-in。

`wait/1` 和 `wait_t/2` MUST 保持 public API、阻塞行为和正常 Outcome，由库内固定 exported external
completion 把 Outcome 返回既有 wait driver。旧 `run_cc`/`run_with_cc`/`exec_cc`/`run` 和 ordinary
anonymous Callback forms MUST 保持 source、argument 和 return normalization，通过 ordinary `sf_cc`
completion descriptor 接入同一 node take/cleanup infrastructure。ordinary final fun 自身不获得 external-fun 的强
upgrade guarantee；需要该保证的用户仍使用显式 sf-cc runner。

stable callback 与 ordinary callback 使用各自明确的 encoding，但都显式保存 direct `parent_ref`。
ordinary Outcome 仍进入 current `cc`；可精确归属到 current `cc`/frame 本身不可调用的 `badfun`/`undef`
才被转换为 failure并向直接 `parent_ref` 转发。已经进入用户 callback 后的内部 exception 原样传播，
不得误分类。

## Nested sf_do

由于 `sf_do` 返回原 Monad，nested block 只是外层 Action：

```erlang
sf_do(outer, [
    async_m
 || run_inner,
    InnerResult <- sf_do(inner, [async_m || inner_step, inner_action(), final_action()]),
    continue_outer(InnerResult)
]).
```

outer `bind_frame` 运行 inner Monad 时，把 outer generated frame 物化为 inner 的直接 `parent_ref` target。
inner action 挂起时 callback 只保存 inner next frame和 `OuterRef`；outer frame 单独存在于
`OuterRef` entry。inner 完成后结果
进入 outer frame。无需 `{sf_subprogram, ...}`、入口 frame 或专用 frame stack VM。

nested identity 使用 lexical path，例如：

```text
[outer, inner]
```

匿名 nested component 使用原始源码 owner/occurrence，不能使用 generated `$sf_.../2`
function 作为 owner，否则 parent identity 变化会级联污染 child identity。

## Pattern Semantics

`classic_do` 对非变量 generator pattern 生成成功/fail 两个 anonymous continuation clauses。
`sf_do` 不生成该 continuation fun，而是在 external frame function 内用 `case Input of` 表达相同的
成功/fail 分支：

```erlang
frame(Input, _Env) ->
    case Input of
        {Expected, Value} ->
            next_monad(Value);
        _ ->
            monad_fail:fail(monad_badmatch, Monad)
    end.
```

和 `classic_do` 的 anonymous fun clause 一样，generator pattern 中的 variables 是该 pattern
的新 bindings；即使 enclosing scope 已有同名 variable，也不构成 pinned equality constraint。
使用 `case` 是为了在单一 external frame function body 内表达 pattern success/fail，而不是改变
pattern binding semantics。

规则：

- variable `<-` pattern 总是绑定 Input；
- non-variable `<-` mismatch 返回 `monad_fail:fail(monad_badmatch, Monad)`；
- unbound action 使用 `then_frame` 和忽略 Input 的 frame；
- `Pattern = Expression` pure let 保持 Erlang `badmatch` 语义；
- 最后一个 qualifier 必须是 Monad action，例如 `return(A)` 或其他 do Action；它直接成为
  remainder，不额外包装 terminal frame；
- 最后一个 qualifier 不能是 `<-` generator 或 pure match。

### Scoped return/fail

`sf_do` 必须保留 `classic_do` 的 block-scoped local-call 语义：

```text
return(Value) -> monad:return(Value, CurrentMonad)
fail(Reason)  -> monad_fail:fail(Reason, CurrentMonad)
```

这里统一进入 typeclass dispatcher，不能把 `return/1` 固定解释为某个 async-specific constructor，
也不按 Monad AST shape 直调 module。rewrite 只作用于 `sf_do` lexical subtree
内的 local `return/1` 和 `fail/1`：remote call 以及 block 外的同名调用保持原样。
只有 block 使用 local `fail/1` 或 non-variable generator pattern 时才需要 `monad_fail` capability；
transform 不预检 capability，缺少时由运行时 dispatcher 按标准方式失败。

nested `sf_do` 进入时压入 inner Monad、离开时弹出，因此 inner block 中的 local call 使用 inner
Monad，outer remainder 恢复使用 outer Monad。第二遍遍历必须在生成 frame body 前完成这项 scoped
rewrite，使 suspended continuation 中不存在待解释的 do-local `return/1`/`fail/1`。

## Timeout, DOWN and Ordinary Failure

timeout、DOWN 和普通 reply 都由 existing router 产生 action outcome，然后传给当前 generated
frame：

```text
timeout/DOWN/reply -> invoke NextFrame(Outcome)
```

它们不直接触发 stable terminal abort。后续行为由 source pattern 和 Monad 决定：

```erlang
Reply <- Action
```

会把 `{error, timeout}` 等普通值绑定到 Reply；

```erlang
{ok, Reply} <- Action
```

收到 error outcome 时 pattern mismatch 并调用 Monad fail。message outcome 不消费 final
continuation，也不推进 frame。

protocol/manifest/MFA 损坏等结构错误仍属于 runtime failure，不与普通 action failure 混淆。
executor 必须把该 failure 交给当前 stable node 的直接 `parent_ref`；普通 success frame 跳过，显式
failure handler 可处理，否则继续向外传播。
`sf_do` transform 不对 Action、Env 或用户 final Context 的任意 runtime term 做 stable-term validation。

## Lexical Capture Analysis

transform 不捕获 enclosing function 的所有 bindings。每个 boundary 的 Env 是目标
continuation 的 closure environment：

```text
Capture(Frame_i) =
    FreeVariables(Continuation_i)
    ∩ LexicallyBoundVariablesBeforeBoundary_i
```

分析必须覆盖：

- suffix actions、let expressions、guards；
- nested fun/case/try/comprehension 的 lexical scopes；
- Monad dispatch 所需的 lexical value；
- generator pattern 新 bindings 与 Input 的排除。

例：

```erlang
Expected = ok,
Unused = value,
{Expected, Reply} <- request(),
finish({Expected, Reply}).
```

等待 request 的 frame 不捕获 enclosing `Expected` 或 `Unused`；pattern 中的 `Expected` 和 `Reply`
都来自 Input。若 suffix 通过另一个未被 generator pattern shadow 的名字引用 enclosing value，
该 binding 才按普通 free-variable analysis 进入 Env。

Env 直接使用 source variable atom 到 runtime value 的 map：

```erlang
#{'UserId' => UserId, 'Token' => Token}
```

scope-aware analysis 仍必须按 Erlang lexical semantics 判断哪个 binding 是 continuation 的 free
variable，并排除 generator pattern 新建的 bindings；这决定“捕获什么”。一旦捕获集合确定，变量
atom 就足以作为 key。在同一个 frame 的可见 lexical environment 中，一个变量名至多指向一个
binding；不同 frame 各自拥有 Env，不需要把 binder origin 编入 key。transform 自己使用的 Input、
Env 和临时变量必须 hygienic，不能占用 source variable key。

binding classifier 必须区分两类 clause pattern：`fun`/named-fun clause parameters 创建 fresh
bindings；`case`、`receive`、`try` clause pattern 中已经在外层绑定的变量是 equality reference，
必须作为 continuation free dependency 捕获。Pass 1 scope collection、capture analysis 与其他需要
理解 lexical binding 的分析必须复用同一分类规则，不能各自维护会漂移的 clause 语义。

CaptureLayout 按 variable atom 排序，用于确定性诊断和 required-capture schema；它本身就是
`sf-migrate` 使用的 CaptureAbi，不再另做 capture hash。CaptureLayout、runtime values 均不参与
named boundary identity。变量改名会表现为旧 key 缺失；跨版本把同名变量改成不同语义默认仍会复用旧值。这两类
source compatibility 由显式 `sf-migrate` Env migrator 处理，不由 `sf_do` 推断。

### Anonymous/local fun capture guidance

Env 原则上不应保存 anonymous/local fun。hot-code upgrade 不会遍历 State/Env 并清除或重写 fun；
它仍是原 term，并引用创建它的 module code version。即使对应 fun body 的源码文本未变化，也不能
据此认为该 term 已指向新版本；可靠边界是旧 code 是否仍被 VM 保留。旧 code purge 后，调用该
local fun 可能失败。external fun/MFA 或纯数据 descriptor 不具有同一 local-code capture 问题。

`sf_do` 只保证 transform 自己生成的 bind/then continuation 是 external frame，不保证任意 capture
value 不含 fun，也不递归检查或拒绝 Env。若应用有意捕获 local fun，应在每次可能 purge defining
code 的 upgrade/downgrade 中，通过 `sf-migrate` Env-only migrator 用 target code 重新生成它，或
迁移为 external MFA/data descriptor。该检测、声明强制和自动诊断是可选后续能力，不属于当前
`sf_do` 必须实现的合同。

transform 只能保存当前 continuation 所需变量，不能预测 future continuation。目标版本新增
前置 binding 依赖时：

```text
TargetRequiredCaptures ⊄ StoredEnv
```

若 `sf-migrate` Env migrator 未在恢复前生成 TargetEnv，则不得执行 target frame，并把 failure 转发给
直接 `parent_ref`；具体 migration Outcome 不由 sf-do 定义。

## Identity

每个具有 block-local remainder 的非最终 monadic qualifier MAY 由前置 inline atom marker 获得一个
source StepName。缺少 marker 时 transform 产生 warning，并生成只能普通执行、不能 migration 的
unnamed frame。pure let 和 final action 不消费 StepName。实现 MAY 保留 source-order index 作为
诊断或 unnamed local-frame metadata，但它不得参与 migration identity 或 target 推断。final action
仍可按 Monad 自身语义 immediate completion 或 suspend，
但它没有 `sf_do` block-local continuation，因此不生成 frame identity；运行时直接保留既有
直接 `parent_ref`。若 final action 挂起，保存的仍是该 Ref；
`sf_do` 不为它人工制造 boundary、identity 或 wrapper continuation。

named migration identity精确为`{Protocol, Block, StepName}`。Action AST、source annotation、lexical parent、
调用位置、variable names、map order、source-order index、pattern、suffix、CaptureLayout、runtime Env和
continuation structure都不参与 identity，因此transform不需要Action canonicalizer、AsyncForm digest、
hash或hex encoding。

generated function atom直接使用`'$sf_<block>__<step>'`，arity固定为2。例如`order_flow`与
`after_payment`生成`'$sf_order_flow__after_payment'/2`。module-local collision check必须拒绝不同
Block/Step拼接出相同atom、与用户函数冲突或名称超过runtime限制；不得truncate或fallback到hash。影响
frame ABI、identity或resume语义的全局不兼容变更通过bump Protocol表达，默认protocol为`1`。

unnamed frame 没有 migration identity。实现仍须为它生成 module-local external MFA 和 runtime frame
locator，以支持普通执行和同版本 resume；current manifest entry 必须明确记录
`step_name => undefined`。migration resolver 遇到该 entry 时必须在 target invocation 前 fail closed，
不得按 locator、source index、Action AST 或邻近 named step 推断 target；具体 failure Outcome 由
sf-migrate 提供并通过 sf-cc direct `parent_ref` dispatch 提交。

## High-Level Implementation Design

### Two AST-aware passes

transform 使用两遍 Astranaut traversal。两遍都是线性遍历，总成本仍为 `O(N)`；它们不是重复做
同一种递归，而是把“读取稳定 source facts”和“产生 executable AST”分开。

```text
Pass 1 — collect original AST, never rewrite
  -> source function owner and lexical sf_do stack
  -> occurrence id, lexical traversal path, Monad and original qualifiers
  -> bindings, inline StepName markers, unnamed warnings and source metadata

Global analysis — no traversal mutation
  -> captures, named identities/local frame locators, generated names and frame plans
  -> nested/name/function conflicts

Pass 1 error gate
  -> accumulate expected source diagnostics in astranaut_return
  -> any error: astranaut_return:to_compiler/1 and stop
  -> no error: enter Pass 2

Pass 2 — rewrite by frozen plan
  -> scoped local return/fail rewrite
  -> inner sf_do first, then outer qualifier lowering
  -> direct bind_frame/then_frame expressions and generated external functions

Finish — outside the traversal
  -> append generated functions, exports and current manifest
```

Pass 1 保留完整 original AST，因而 global analysis 可以一次性看到所有 block，并在任何 replacement
改变 tree shape 之前完成 identity、capture 和 collision 校验。Pass 2 不重新发现 boundary、不重新
校验 source form/name-marker grammar、不重新计算 identity/capture，也不对已经生成的 function 做第三次 source
analysis；它只消费冻结的 frame plan。

### Stable occurrence matching

Erlang AST term 没有 object identity，source annotation 也未必唯一。因此两遍不能靠“相等 AST”或
line number 猜测同一个 `sf_do`，而应按相同的 lexical pre-order 规则分配 occurrence id：

```text
OccurrenceId = {SourceFunction, LexicalPath, OccurrenceIndex}
```

Pass 1 保存 occurrence 对应的 original-node digest 和 plan；Pass 2 重新遇到 occurrence 时按 id
取 plan，并校验 digest。数量、顺序或 digest 不一致是 transform ordering/source occurrence
mismatch，必须报错，不能 silent fallback。未包含 `sf_do` 的 function 可以在 Pass 2 跳过，作为
不改变语义的优化。

所有可预期的 source validation error 必须由 Pass 1 callback 返回为 Astranaut error result，而不是
`throw`/`error`。Pass 1 应完成可继续的遍历以累计互相独立的 diagnostics；完成后若结果包含任何
error，入口直接调用 `astranaut_return:to_compiler/1`，不得初始化或运行 Pass 2。unexpected internal
fault 可以按 compiler internal error 单独报告，但不得伪装成普通 source diagnostic。

### Diagnostic ownership and formatting

`sf_do` diagnostic 分为三个所有者：

```text
async_sf_transform reason/format_error  -> rule identity and domain message
Astranaut return/compiler adapter       -> formatter dispatch and safe fallback
OTP compiler                            -> file, line, column, source excerpt and marker
```

transform reason 不得复制 offending AST、source text、line/column 或完整 analysis/rewrite state；compiler
已经根据 diagnostic position 展示原代码。领域消息只解释违反的规则、允许的 source form，以及确有
区分价值的动态身份（例如 duplicate StepName、Block 或直接冲突的 generated function）。

`async_sf_transform:format_error/1` 必须是严格的领域 formatter：每个 transform-owned reason 使用精确
clause，未知 reason 保持普通 Erlang `function_clause`，不得定义 generic catch-all，也不得在 callback
内部调用 shared dispatcher。正常 compiler 路径由 `astranaut_return:to_compiler/1` 把 formatter 包装为
`astranaut_lib`；Astranaut 在 formatter 没有匹配 clause 或其 helper 发生 `error:function_clause` 时才对
原始 reason 使用默认 term formatting。其他异常保持原 class/reason/stacktrace。

测试必须分开证明两层语义：实际 compiler diagnostics 中的每个 transform-owned reason 都直接调用
领域 `format_error/1`，从而让拼写错误或遗漏 clause 以 `function_clause` 失败；未知 reason 的生产兜底
则单独通过 `astranaut_lib:format_error/2` 验证。领域消息测试断言精确文本，compiler integration 测试
断言 reason 与 `{Line, Column}`，不复制 OTP source excerpt 的版本相关渲染。

纯 `format_error/1`、Astranaut fallback 和 handcrafted AST 的 `parse_transform/2` 防御性检查属于
EUnit。需要调用真实 OTP compiler、观察 source-position diagnostics、验证多错误累计或执行生成代码的
场景属于 Common Test；不得为了共享 assertion macro 把模块级单元测试放进 CT suite。

错误累计只针对独立 source violations。同一 invalid block 已报告 duplicate StepName 或 duplicate
Block 后，不得再为其派生 generated function conflict；trailing StepName marker 已足以
说明缺少后继 non-final action 时，也不得同时为前一个 generator/match 产生派生的 invalid-last 错误。
真正与用户已定义 `Function/2` 冲突的 generated function 仍是独立错误。

`empty` comprehension 对普通 Erlang source parser 不可达，但作为较早 transform 在合法 function form
内提供 malformed AST 时的防御性 validation 保留。function 外 application 不是合法 compiler form，
不会进入 AST traversal，因此不保留不可达的领域 reason。Pass 2 ordering mismatch 是 internal
diagnostic：payload 只能包含有界摘要或 occurrence identity，不能包含 frozen plans、generated AST 或
完整 rewrite state。

### Nested rewrite order

Pass 2 在进入 `sf_do` 时按 occurrence id 取得 plan，并把当前 Monad/plan 压入 lexical stack；
child traversal 完成 local `return/1`、`fail/1` rewrite，并在 nested `sf_do` 的 post callback 中
先生成 inner expression。随后 outer `sf_do` 的 post callback 使用：

- Pass 1 冻结的 original qualifiers 做 capture、name pairing 和 source metadata lookup；
- Pass 2 已转换的 child expressions 生成实际 remainder/frame body。

这样执行 AST 是由内到外生成的，但所有语义事实已经在第一遍从稳定的 original AST 收集完成。
nested block 不会被当作 generated function 的新 source occurrence，也不会在第二遍重复分析。
第二遍不得重新调用 source-form、name-marker、identity 或 capture validator；生成 executable AST
需要的改写后 qualifiers 必须与冻结 plan 组合使用，而不是重建另一份分析结果。

### Qualifier lowering boundary

对一个 block，从 final Monad action 向前构造 frame sequencing chain：

```text
final Monad action             -> action itself
Pattern <- Action, Remainder   -> monad_frame:bind_frame(Action, PatternFrameMFA, Context, Monad)
Action, Remainder              -> monad_frame:then_frame(Action, IgnoreInputFrameMFA, Context, Monad)
Pattern = Expr, Remainder      -> match followed by Remainder inside the frame body
```

最后一个 qualifier 若为 local `return(A)`，先按当前 Monad rewrite，再作为 final action 返回；若为
其他 Monad action，同样直接返回。和 `classic_do` 一样，final action 不经过 `bind_frame` 或
`then_frame`，也不生成 StepName、frame function 或 manifest entry。terminal generator
或 pure match 没有 remainder，必须在 transform 阶段拒绝。

不能用 generic tuple/list recursion 代替 AST traversal。identity 必须从 Pass 1 冻结的
Block/StepName 产生；unnamed locator 不能被当作 migration identity，也不能从 generated AST
或 Action 内容推导 migration target。把 original/lowered AST
同时携带在单遍 state 中理论上可行，但会把 global validation、nested scheduling 和 code
generation 耦合在同一 callback 中，不作为首选实现。

## Manifest

transform 固定生成并 export `'$async_sf_manifest'/0`。current manifest 描述当前可恢复 boundary：

```erlang
#{
    format => 1,
    protocol => 1,
    module => Module,
    vsn => Vsn | undefined,
    blocks => #{
        Block => #{
            steps => #{
                StepName => #{
                    mfa => {Module, Function, 2},
                    required_captures => RequiredCaptureLayout
                }
            }
        }
    }
}.
```

manifest 的 `vsn` 沿用 explicit module `-vsn` literal term；无可用显式值时为 `undefined`。
manifest MAY 另存 unnamed local-locator entries以支持普通执行/同版本resume，但不得把它们放入named
`blocks/steps` identity namespace。manifest不保存hash/AsyncId、full source AST、runtime Env、user sf-cc
descriptor、migration selector或historical mapping。

## Callback and Runtime Ownership

ordinary/stable action callbacks、intermediate continuation nodes 与 outer callback descriptors 共用 callbacks map。
ordinary entry保存current anonymous `cc`、`acc_ref`、direct `parent_ref`及其余action lifecycle metadata。
stable entry是完整`#sf_callback{}`，保存：

- 当前 `sf_frame`；
- accumulator reference；
- 直接 `parent_ref`；
- timer reference。

两种 action entry 都不得保存完整 downstream chain 或重复的 root/final Ref。ordinary `#callback{}` 不得被
runtime猜测为 stable frame；其 `parent_ref` 只提供 current `cc` availability fallback，不提供 frame migration。

request correlation key 和 monitor 仍由 existing request/reference router 拥有。frame 更新或
migration 不得重发 request、重建 monitor 或重新开始 timeout。final reply/timeout/DOWN 的
existing routing path 负责决定 action lifecycle；每个 continuation node 的 take、其直接资源清理、
failure forwarding 和最外层用户 callback invocation 由 sf-cc 完成。

一个 Promise 的 nested/parallel implementation 可以物化多个 internal callbacks 或共享 outer nodes；
每个 entry 仍由自己的 RequestMRef 路由。共享节点的 exactly-once、反向查找和取消由 sf-cc 的独立
索引/ownership 规则维护，不得通过在每层复制 root Ref 表示。

## Upgrade Compatibility Boundary

每个 frame invocation 前都必须按其 owner Module/Vsn 进入 resolution boundary。无需转换的直接恢复
仍至少检查两层：

```text
identity compatible:
    frame has StepName
    and target manifest contains same Protocol/Block/StepName and generated MFA

environment compatible:
    TargetRequiredCaptures ⊆ StoredEnv keys
```

identity compatible 但 Env 缺失仍不能执行。runtime 不猜测或重放前置逻辑；resolver 可以使用
sf-migrate 的 optional Env converter，并在 Input 到达后使用 optional Input converter。没有 converter
时分别复用 StoredEnv 和原 Input。StoredEnv 多余字段不构成不兼容。

同 module 其他 block、lexical extraction、Promise传递、pending Action AST、pending action 后面的代码和
capture runtime values不改变named identity。改变Block或StepName需要显式frame mapping，否则resolution失败；
没有 StepName 的 historical/current frame 在 migration 中默认不兼容，runtime 不按 local locator、
index 或 AST 猜测 target。

upgrade resolver 可以只使用 actual stored source frame 与 current target manifest；historical old block
metadata 不是运行必要条件，只能提供更早 diagnostics。strict downgrade 是否要求 historical artifact、
Env/Input converter protocol、version-chain composition 和 failure Outcome shape 均由 sf-migrate 定义。

owner module 独立升级不会自动触发 holder process 的 `code_change/3`，因此 correctness 不能依赖 eager
migration。reply-time 必须在调用每个 frame MFA 前 lazy resolve；linked nodes 中不同 owner modules 的
frame 分别 resolve，尚未执行的 nodes bit-for-bit 保留。resolution 无法继续时把 failure 交给当前
node 的直接 `parent_ref`，不得跳过中间 failure handler 或寻找 root/final Ref。
