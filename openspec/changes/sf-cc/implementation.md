# sf-cc Implementation Contract

## 1. Responsibility Boundary

### `async_sf_cc_macro`

负责 `run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5` external-fun source validation、diagnostics 和
macro expansion。它保留原 external-fun AST，不分析 runtime Promise/Context/State。

### `async_sf_cc`

负责统一 `ContinuationRef` node constructors/classification、逐节点 dispatch、take-before-invoke、direct
resource cleanup、outer callback invocation、inspection 和主动 failure。

### `async_t`

负责 action suspension writer、reply/message/timeout/DOWN routing、显式 `(K,SfContext)`、`bind_frame`
parent materialization，以及 ordinary/stable action entry dispatch。

### `async_sf_runtime`

负责 current `sf_frame` validation/resolution/invocation。它只接收 current frame 和 direct `parent_ref`，
不得遍历或构造递归 continuation chain。

### `async_r_t` / `async_util` / `async_m`

- `async_r_t` 独立传播 AccRef 与 current continuation context；
- `async_util` 提供 callbacks backend 和 shared value classifier；
- `async_m` 提供 public runners、inspection/failure facade 和现有 result projection。

## 2. Shared Store Encoding

ordinary action callback 使用：

```erlang
-record(callback, {
    cc,
    acc_ref,
    parent_ref
}).
```

stable action callback 使用独立 encoding：

```erlang
-record(sf_callback, {
    frame,
    acc_ref,
    parent_ref,
    timer_ref = undefined
}).
```

精确字段可由 shared internal header扩展 monitor/resource metadata，但以下语义不可改变：

- `cc`/`frame` 只表示 current reply consumer；
- `parent_ref` 只指向 current `cc`/`frame` 的 direct continuation parent；
- continuation payload只有current `cc`/frame和direct `parent_ref`；entry仍完整保存`acc_ref`和timer等
  action lifecycle metadata；
- entry 不保存 outer frame副本、recursive downstream 或 root/final Ref；
- ActionMRef 是 ordinary/stable callback 的 external reply key；intermediate node使用内部 Ref key；
- ordinary `#callback{}` 不被猜测或升级为 `#sf_callback{}`，但两者都必须保存显式 `parent_ref`；
- 缺少 `parent_ref` 的旧 callback tuple 不属于本 change 的 reader compatibility boundary。

## 3. Unified ContinuationRef Nodes

```erlang
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

FinalCcRef 与 OuterRef 不得成为不同 reference types。value shape 决定 executor，`parent_ref = none`
决定链结束。node absence 是 consumed marker；不得创建 done tombstone。

`sf_cc` callback/context contract保持：

- `async_r_t` mode：external `/2` 返回 `async_r_t`；
- `direct_state` `/2` return ignored；
- `direct_state` `/3` 接收 CleanState并返回 compatible NewState；
- ordinary runner MAY 使用 library adapter；external-fun source restriction只适用于 sf-prefixed macros。

## 4. Runner Construction

所有 public runners委托 common core：

1. allocate outer `ContinuationRef`；
2. 注册 ordinary/library/sf_cc outer node；
3. 用该 Ref作为 current parent运行 Promise；
4. immediate completion在同一 evaluation内 dispatch/take outer node；
5. ordinary/stable suspension都在各自 action callback中保存 direct parent Ref。

`run_cc`、`run_with_cc`、`exec_cc`、`run`、`wait` 保持既有 arguments、normalization 和正常结果。
它们注册 ordinary/library outer node，不获得 external-fun source guarantee。

## 5. Explicit Continuation Context

`async_t` internal action MUST 接收 `(K,SfContext)`，不得使用进程字典。`SfContext` 表示：

```erlang
undefined
| #{current := {frame, SfFrame} | {ordinary, Fun} | {ref, ContinuationRef},
    parent_ref := ContinuationRef | none,
    runner := fun()}.
```

- ordinary `>>=` 继续创建 anonymous `K`并透传 current `parent_ref`；
- Promise 仅在实际 `K =:= SfContext.runner` 时写 stable `#sf_callback{}`；否则写
  `#callback{cc = K, parent_ref = ParentRef, ...}`；
- `bind_frame(Action,Frame,...)` 若 current parent尚不可寻址，先物化 ParentRef；
- 新 Action在 `{current = Frame, parent_ref = ParentRef}` 下执行；
- nested bind每次只物化一层，因此 dynamic case/try/nested path无需静态枚举；
- ordinary downstream被物化为 ordinary node，不嵌入 stable frame data。

## 6. Node Dispatch

统一 primitive：

```erlang
dispatch_continuation(Ref, Outcome, OffsetOrLens, State) ->
    {ok, DispatchValue, NewState}.
```

每次 dispatch：

1. take node；Ref此后即 consumed；
2. cancel/demonitor/remove该 node直接拥有的 resources；
3. 得到 CleanState；
4. 根据 Outcome/node kind执行下表；
5. take后的失败不得恢复或重试 Ref。

internal dispatcher只在Action callback已成功路由后调用，因此ContinuationRef缺失是 lifecycle invariant
failure，直接抛出`{invalid_continuation,{missing_node,Ref}}`。它不得借用external router的`unhandled`。

| Node | success | failure |
|---|---|---|
| stable success frame | resolve/invoke frame，returned Monad运行于 `parent_ref` | 不调用 frame，dispatch `parent_ref` |
| explicit failure handler | 按 handler contract | handler MAY consume或产生新 Outcome |
| ordinary continuation | 调用当前 continuation；不得在正常返回后额外 dispatch `parent_ref` | 仅当 current continuation 本身不可调用时 dispatch `parent_ref`；若无 parent则按 ordinary outer contract |
| outer sf_cc | invoke/normalize callback | invoke/normalize callback |

frame protocol/manifest/MFA/migration failure使用规定 Outcome并只向 direct `parent_ref` 转发。dispatcher不得
搜索 root、跳过多个节点或直接定位 final callback。

## 7. Exception Boundary

只有能够精确归属到“当前 continuation 本身不可调用”的 `badfun`/`undef` 才转换为 failure并向
`parent_ref` 外移一层。对 ordinary action，runtime MAY 捕获 `error:{badfun, Cc}`，但其中 term MUST 与
正在调用的 `#callback.cc` 完全相同。
一旦已经进入用户 callback，其内部调用产生的 `badfun`、`undef` 或其他 exception保持 callback contract，
不得误分类为 continuation availability failure。

external sf_cc callback MAY 在实际 apply 前使用 `code:ensure_loaded/1` 与 `function_exported/3` 做精确
availability check。不得捕获 nested invocation exception。

## 8. Exactly-Once and Cleanup

exactly-once以每个 node为单位，而不是按重复 root CcRef批量完成：

- node在调用或转发前 take；
- late/duplicate external reply由精确router分类为`missing_callback`，legacy API可映射为`unhandled`；
- batch snapshot中被earlier sibling cleanup删除的action记录`already_removed`；
- 已成功路由Action后发现ContinuationRef absent直接抛出`invalid_continuation`；
- callback crash/invalid return不恢复 node；
- direct `/3` 只看到 current node direct cleanup后的 CleanState。

continuation execution只需要 current node 的 direct `parent_ref`。主动 cancellation或 parallel teardown若需从 outer node反向找到
children，MUST 使用独立 reverse index/ownership metadata，并满足：

- index不参与 continuation execution semantics；
- index mutation与 node registration/take保持一致；
- 不允许为简化 cleanup而在每个 node复制 root/final Ref；
- orphan cleanup、shared parent和cycle detection有显式 tests。

## 9. Message, Timeout and DOWN

- message不消费 ActionMRef或 continuation node；
- reply、timeout、DOWN按既有 action lifecycle决定是否消费 ActionMRef；
- ordinary timeout/DOWN Outcome默认作为 current `cc`/frame Input；
- 只有 ordinary `cc` 本身或 frame infrastructure不可调用 current continuation时才进入 failure forwarding。

## 10. Store Classification

shared classifier MUST 区分：

```erlang
{stable_action, SfCallback}
{ordinary_action, Callback}
{intermediate_node, Node}
{outer_node, SfCc}
{accumulator, Value}
other
```

external reply router只执行 stable/ordinary action entries；intermediate/outer nodes只能由 internal dispatcher
执行。`callback_exists/1` 只统计 external pending action entries。map/list/dict backends使用相同语义。

## 11. Inspection

默认 sanitized summary：

```erlang
#{
    request_mref := term(),
    stable := boolean(),
    parent_ref := reference() | none,
    continuation_kind := ordinary_fun | sf_frame | unknown,
    frame := #{
        module := module(),
        vsn := term() | undefined,
        protocol := integer(),
        block := atom() | undefined,
        step_name := atom() | undefined
    } | undefined
}.
```

summary不公开 Env、State、timer、monitor、accumulator、continuation payload、sf_cc callback或Context。
默认只报告 current frame。若提供 chain view，MUST 显式沿 `parent_ref` lookup并限制 cycle、depth和invalid shape。

matcher snapshot/sort/preflight必须在mutation前完成；matcher exception原样传播，non-boolean result报
`{invalid_matcher_return,Value}`。

## 12. Active Failure

`fail_sf_callbacks/4` 对selected stable entry注入：

```erlang
{error, {sf_aborted, Reason}}
```

commit按selected Ref term order。current node被消费后，failure只dispatch direct `parent_ref`。ordinary
entry返回`ordinary`且不删除。result status为
`forwarded | handled | already_removed | ordinary`。shared outer node遵守
normal exactly-once take，不按root去重。

## 13. Metadata and Vsn

frame summary保存 Module、literal module Vsn或`undefined`、Protocol、Block和StepName。
不得公开 Env；不得把 OTP attribute container `[Vsn]` 当成literal Vsn。

## 14. Required Tests

- macro external `/2|3` acceptance/rejection；
- immediate/suspended ordinary和stable Promise；
- `InnerFrame + OuterRef` linked representation，无recursive `sf_next`/`sf_cc_ref`/root duplication；
- bind_frame自动parent materialization与deep case/nested path；
- sequential success逐node恢复；
- structural/migration/current-continuation `badfun`逐层failure forwarding；
- ordinary success frame在failure path被跳过，explicit handler可消费；
- node take-before-invoke、late reply、duplicate fail、shared parent exactly-once；
- timer/message/DOWN/monitor/accumulator direct cleanup；
- parallel reverse-index cancellation和orphan cleanup；
- stable/ordinary/intermediate/outer classifier；
- current-only inspection、explicit safe chain traversal、matcher preflight atomicity；
- external callback availability不捕获callback-internal `badfun`/`undef`；
- map/list/dict backends、compile、EUnit、Common Test、xref、dialyzer和supported OTP matrix。
