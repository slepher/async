# sf-do / sf-migrate 讨论预览

> 状态：讨论预览；其中 `sf-cc` lifecycle 已同步到 `openspec/changes/sf-cc`，`sf-migrate`
> 仍包含未冻结部分。
>
> 本文记录本轮关于 legacy promise、最终 CC、`CcRef`、主动失败和
> `sf-migrate` 的讨论结论，同时列出阻止直接实现完整方案的缺口。现有
> `openspec/changes/sf-do` 已同步本轮 CC 结论，`openspec/changes/sf-migrate` 尚未完整重写；发生冲突时，
> 已校验的 OpenSpec change 是相应 capability 的正式依据。

## 1. 问题定义

这里所说的“旧 promise”特指 callbacks 中保存匿名 continuation fun 的 legacy promise，
不是“由旧版本 `sf_do` 生成的 stable frame”。当前实现把 fun 或 stable map 都保存在
`#callback.cc`：

```erlang
-record(callback, {
    cc :: fun((A) -> async_r_t:async_r_t(any(), any(), monad:class(), A)) | map(),
    acc_ref :: reference()
}).
```

promise 挂起以后，原 Erlang 调用栈已经结束。匿名 continuation fun 在 appup、旧代码 purge
或依赖模块独立升级之后可能不能调用；它也是不可解析的闭包，runtime 无法从中恢复业务调用栈、
目标 step 或最终 completion CC。

因此总会存在无法通过 migration 恢复的情况：

- 没有匹配的 migration；
- Env 或 Input migrator 失败；
- legacy anonymous continuation 已失效；
- stored frame 的 manifest/MFA 已失效；
- protocol 或 continuation shape 不受支持；
- 用户在 `code_change/3` 或运行时主动要求立即失败。

发生上述不可恢复错误时，runtime 不应尝试重建已经消失的调用栈。它应丢弃或清理不可用的
continuation，直接把失败 Outcome 交给 promise 最终的 CC。

## 2. 已确认的术语和边界

### 2.1 `RequestRef`

`RequestRef` 是单个异步 request/reply 的 correlation key，即当前 callbacks map 的普通 key。
reply、timeout、DOWN 和 message 仍按它路由。

### 2.2 `CcRef` / CC MRef

`CcRef` 是内部变量名，语义上就是 CC MRef。它是某个通过最终 CC 运行的 promise
的一次性最终完成地址。它不是多个 promises 的
operation group identity，也不表示内部并行 branch 的业务分组。

无论这个 promise 之前的实现使用：

- anonymous continuation fun；
- `sf_do` stable continuation；
- nested continuation；
- 内部并行和聚合；

最终只产生一个 Outcome，并消费一个 `CcRef`。

如果实现层为一个复合 promise 物化了多个内部 callback entries，它们可以通过
`#callback.sf_cc_mref` 持有相同 `CcRef` 以便路由或清理，但这不意味着每个 entry 都拥有一个
独立的最终结果，也不意味着最终 CC 会按 branch 调用多次。

### 2.3 最终 `sf_cc`

用户不直接提供内部 `sf_cc` map。独立于 `sf_do` 的 sf-cc macro 有两种 runner
形式，都接收 external-fun source expression 和用户 Context：

```erlang
%% async_r_t runner：只接受 callback /2，整个 runner 返回 async_r_t
run_sf_cc(Promise, fun Module:Function/2, Context).

%% 直接 State runner：升级现有 run_with_cc，接受 callback /2 或 /3
run_with_sf_cc(Promise, fun Module:Function/2, Context, Offset, State).
run_with_sf_cc(Promise, fun Module:Function/3, Context, Offset, State).
```

其中 `Context` 在注册 promise 时预先保存。两种 runner 的 callback 合同不同：

```erlang
%% async_r_t runner
Module:Function(Outcome, Context) -> async_r_t().

%% run_with_sf_cc direct-State runner
Module:Function(Outcome, Context) -> Ignored.
Module:Function(Outcome, Context, CleanState) -> NewState.
```

async_r_t runner 只接受 `/2`；需要 State 时，callback 从它返回的 `async_r_t` effect 中获取，
因此不接受 `/3`。`run_with_sf_cc` direct-State runner 接受 `/2` 和 `/3`：`/2` 的直接返回值被
忽略，State 保持为清理后的值；`/3` 的返回值作为 NewState。`/3` 接收的 `CleanState`
是最终 CC 调用前已经清理 ActionMRef、CcRef 和相关 continuation 资源的调用时 State，
不是注册时预存的 State。

macro 的职责仅限于在编译期分析第二个参数。`run_sf_cc/3` 只允许 arity 2；
direct-State `run_with_sf_cc/5` 允许 arity 2 或 3。该参数必须是 remote/external fun AST：

```erlang
{'fun', Pos, {function, ModuleExpr, FunctionExpr, {integer, _, Arity}}}
%% Arity =:= 2 orelse Arity =:= 3
```

以下 source forms 必须在 callback 参数位置产生 compile error：

- `fun LocalFunction/2` 或 `fun LocalFunction/3`；
- anonymous fun；
- `run_sf_cc/3` 中 arity 不是 2 的 external fun；
- `run_with_sf_cc/5` 中 arity 不是 2 或 3 的 external fun；
- 任意普通 runtime expression。

成功时 macro 保留原 external-fun expression，并按 runner mode 改写为对应的普通函数调用：

```erlang
%% ExternalFunExpr 是原样保留的 fun Module:Function/Arity AST
async_m:run_sf_cc(Promise, ExternalFunExpr, Context).
async_m:run_with_sf_cc(
    Promise,
    ExternalFunExpr,
    Context,
    Offset,
    State
).
```

`async_m:run_sf_cc/3` 和 `async_m:run_with_sf_cc/5` 是普通 runtime functions，它们的第二个
参数仍是 `fun M:F/A` 产生的
external-fun term，不是分拆后的 `Module, Function, Arity` 或 `{Module, Function, Arity}`。
external/local 的 source-form 限制已由 macro 保证；普通函数不能也不需要重做 AST 检查。

runtime 保存该 external fun 本身和预存 Context：

```erlang
SfCc = #{
    kind => sf_cc,
    protocol => 1,
    mode => async_r_t | direct_state,
    callback => ExternalFun,
    context => Context
}.
```

最终完成时 runtime 按 runner mode 和 fun arity 分派：

```erlang
invoke_sf_cc(#{mode := async_r_t, callback := Callback, context := Context}, Outcome, _State)
        when is_function(Callback, 2) ->
    Callback(Outcome, Context);

invoke_sf_cc(#{mode := direct_state, callback := Callback, context := Context}, Outcome, State)
        when is_function(Callback, 2) ->
    _Ignored = Callback(Outcome, Context),
    State;
invoke_sf_cc(#{mode := direct_state, callback := Callback, context := Context}, Outcome, State)
        when is_function(Callback, 3) ->
    Callback(Outcome, Context, State).
```

对 `async_r_t` mode，`/2` callback 的返回值必须是 `async_r_t`；该 mode 不接受 `/3`。
对 `direct_state` mode，`/2` 的返回值不做 State normalization，而是直接忽略；`/3` 必须
返回符合现有 State-type 判定的 NewState，不接受 `async_r_t`。不支持或损坏的 mode/fun/arity/
return shape 应进入 runtime 的 invalid-CC 错误路径。这个 runtime dispatch
只区分调用协议 `/2` 和 `/3`，不再判定该 fun 在 source 中是 external、local 还是 anonymous；
后一项已经是 macro 的编译期责任。

这个静态检查只证明 callback 参数的 source form 是 external fun 且 arity 为 2 或 3；它不证明
`Module:Function/Arity` 在 promise 完成时仍已加载、已导出或可调用。后者是用户指定 external fun
的生命周期责任；普通 runtime function 不为此增加预防性检查。

该 macro 使用项目已有的 Astranaut macro 基础实现，但不属于 `async_sf_transform`，也不属于
`sf_do` source transform。它应有独立 provider/diagnostic ownership，并通过公共 include 启用。

该 external fun（逻辑上仍指向 MFA）由用户显式指定：

- `sf_do` 不自动生成 CC MFA；
- CC 不进入 block `AsyncId`；
- CC 不进入 block migration manifest；
- runtime 不迁移 CC 自身；
- `Context` 是用户在 run 时显式传入并预先保存的 opaque term，例如调用方的 `From`；
- `Context` 不是 `sf_do` 根据 lexical scope 自动分析或捕获的 Env，transform 不推导其字段；
- Env/Input migration 不自动读取、转换或补齐 CC `Context`；
- 用户负责保证 external fun 的目标 MFA 在 promise 存活期间仍可调用，并保证
  `Context` 可安全保留；
- `run_with_sf_cc` `/3` 接收清理后的调用时 State，并返回 NewState。

不引入独立的 `fallback_cc` 用户接口。正常完成和不可恢复失败调用的是同一个 `sf_cc`，由
`Outcome` 区分结果。

### 2.4 旧 CC runners 与新 `sf_cc` runners

当前 `async_m` 已经暴露两层 CC runner：

```erlang
run_cc(Promise, CC) -> async_r_t().
run_with_cc(Promise, CC, Offset, State) -> StateResult.
```

当前内部 `CC` 是 `fun(Outcome) -> async_r_t()`，没有独立 Context、CcRef 或 external-fun
编译期约束。`run_with_cc/4` 创建一个 run-local Ref，以 `remove_ref_after_cc` 在 CC 后清理。

新 stable callback 合同与旧 CC 差异较大：要求 external-fun source form、预存 Context 和可升级的
持久化 descriptor。因此保留旧 API，并在 `cc` 前增加 `sf_` 命名边界：

```erlang
run_sf_cc(Promise, fun Module:Function/2, Context) -> async_r_t().
run_with_sf_cc(Promise, fun Module:Function/2, Context, Offset, State) -> StateResult.
run_with_sf_cc(Promise, fun Module:Function/3, Context, Offset, State) -> StateResult.
```

对应 `exec_cc/4` 的 stable 包装命名为 `exec_sf_cc/5`。旧 `run_cc/2`、`run_with_cc/4`、
`exec_cc/4`、高层 `run/4` 和 `callback_to_cc/1` 保留旧参数、normalization 和正常返回合同，
但升级后通过 legacy final adapter 接入同一 CcRef engine。`wait/1`、`wait_t/2` 也创建 CcRef，
使用 library-owned exported completion 并保持原等待与返回行为。ordinary APIs 不接受新的 external-fun
source constraint；其 anonymous final adapter 也不获得 external fun 的跨 purge 稳定保证。

### 2.5 Ordinary Callback 如何转成旧 CC

当前 ordinary `Callback` 不会直接进入 `run_cc`。`async_m:callback_to_cc/1` 把它规范化为
唯一内部 shape：

```erlang
CC :: fun(Outcome) -> async_r_t().
```

转换规则是：

```erlang
%% Callback/0：忽略 Outcome
CC = fun(Outcome) -> normalize_callback_result(Callback(), Outcome) end.

%% Callback/1：传入 Outcome
CC = fun(Outcome) -> normalize_callback_result(Callback(Outcome), Outcome) end.

%% Callback/2：从 async_r_t 取得当前 State 后调用
CC = fun(Outcome) ->
    State = async_r_t:get_state(),
    normalize_state_callback_result(Callback(Outcome, State), Outcome, State)
end.
```

当前 normalization 大致为：

- callback 返回 async struct：转成 `async_r_t`；
- callback 直接返回 `async_r_t`：原样使用；
- `/2` callback 返回与当前 State 同 record type/arity 的 term：写入 NewState，并保留
  Outcome 作为 result；
- 其他返回值：包装成 `async_r_t:return(Value)`。

因此旧路径的“Callback → CC”是一个 runtime adapter，产生的 CC 仍可能是 anonymous
fun，不能作为新 stable final CC 持久化合同。

### 2.6 External Callback 如何转成 `sf_cc`

新 `sf_cc` 路径不经过 `callback_to_cc/1`，也不生成 anonymous adapter fun。macro 在编译期
验证用户写出的 `fun M:F/A`，然后 runtime 把该 external-fun term、Context 和 runner mode
组装为稳定 descriptor：

```erlang
#{
    kind => sf_cc,
    protocol => 1,
    mode => async_r_t | direct_state,
    callback => ExternalFun,
    context => Context
}.
```

它不是把 ordinary Callback 转换为旧 `CC/1`，而是保留 external callback 本身并把调用
协议数据化。CcRef 指向该 descriptor，`#callback.sf_cc_mref` 只保存 CcRef，因此
legacy continuation fun 或 stable frame 失效时都不需要解析其内部结构。

## 3. 已冻结的 callback-store 模型

以下 shape 已由 `sf-cc` change 冻结；实现可在不改变 observable protocol 的前提下选择内部 helper。

### 3.1 最终 CC entry

顶层 run 注册一个一次性 entry：

```erlang
CcRef => #{
    kind => sf_cc_ref,
    protocol => 1,
    cc => FinalCc
}.
```

`FinalCc` 可以是显式 `sf_cc` descriptor、保持旧 normalization 的 legacy final adapter，或 wait 的
library-owned completion。

### 3.2 Promise callback entry

旧 promise 不改写 continuation，新 frame promise 也不改写 frame data。二者继续使用现有
`#callback.cc` slot，只在 `#callback{}` 尾部增加一个 `sf_cc_mref` 字段：

```erlang
-record(callback, {
    cc :: fun() | map(),
    acc_ref :: reference(),
    sf_cc_mref = undefined :: reference() | undefined
}).

ActionMRef => #callback{
    cc = AnonymousFunOrStableContinuation,
    acc_ref = AccRef,
    sf_cc_mref = CcRef
}.
```

`cc` 的执行仍需按实际 shape dispatch：fun 使用 legacy executor，tagged stable data 使用
`async_sf_runtime`。这个差异只属于恢复实现，不影响最终完成接口。

### 3.3 `#callback{}` 的最小扩展和旧 tuple 兼容

callbacks map 可能跨 code change 长期存在。增加 `sf_cc_mref` 会改变 private record 的 tuple arity，
因此新 reader 必须显式兼容两个 shape：

```erlang
{callback, Cc, AccRef}          % 升级前已存在的 record tuple
{callback, Cc, AccRef, CcMRef}  % 新 record tuple
```

不为 legacy continuation 增加 envelope，不改写其 fun，也不增加 sidecar link。升级后由 wait、ordinary
runner 或 sf-prefixed runner 新创建的 legacy/frame promise，都在 action callback 写入时填充
`sf_cc_mref`。

已经存在于 State 中的旧 `#callback{cc = Fun, acc_ref = AccRef}` 没有 `CcRef`、独立 `sf_cc`、
版本或 source metadata，不能从 closure 中反推出这些信息。新 runtime 继续按旧路径读取它们，
但它们的 `sf_cc_mref` 逻辑值是 `undefined`，不宣称具有新的 terminal fallback 能力。

## 4. 一次性完成与失败

所有最终路径收敛为一个内部原语：

```erlang
complete_sf_cc(CcRef, Outcome, State).
```

已冻结语义：

1. 从 callbacks store 原子 `take` `CcRef` entry；
2. `take` 成功的路径拥有最终调用权；
3. 清理已经无效且属于该 promise 的 continuation、timer、monitor 和 accumulator 数据；
4. runtime 以已完成上述清理的 `CleanState` 调用内部
   `invoke_sf_cc(SfCc, Outcome, CleanState)`；
5. `async_r_t` mode 执行 `/2` 返回的 `async_r_t`；direct-State mode 忽略 `/2` 返回值，
   或把 `/3` 返回值作为 NewState；
6. `CcRef` 已不存在时返回 unhandled，不再次调用 CC；
7. 迟到 reply 只会命中已删除或已失效的 request，不能产生第二次 completion。

正常完成示意：

```erlang
complete_sf_cc(CcRef, Result, State).
```

runtime failure 示意：

```erlang
complete_sf_cc(
    CcRef,
    {error, {sf_runtime, Reason}},
    State
).
```

人工终止示意：

```erlang
complete_sf_cc(
    CcRef,
    {error, {sf_aborted, Reason}},
    State
).
```

### 4.1 哪些错误直接进入最终 CC

候选 terminal conditions：

- legacy anonymous fun 已无法执行；
- stable frame 不能在当前 manifest 验证；
- migration rule 不存在或不能处理实际 source frame；
- Env/Input migrator crash、返回非法值或明确失败；
- unsupported protocol、损坏的 continuation；
- 用户 matcher 选中并要求立即失败。

ordinary业务结果、timeout、DOWN 或业务 `{error, Reason}` 默认仍是 promise Input，由正常
continuation决定如何处理。只有 protected continuation 基础设施不能继续时才跳过它并进入最终 CC。

### 4.2 Legacy fun 的 resume-time 兜底

主动 appup 扫描可以在调用旧 fun 前终止 protected promise。若外部模块独立升级，没有触发
持有 callbacks 的进程执行 `code_change/3`，reply-time executor 仍需以窄分类识别 old-code purge：

- 新四元 callback 有 `sf_cc_mref` 时，跳过不可调用 continuation，并消费 CcRef 进入最终 CC；
- 升级前旧三元 tuple 没有 CcRef 时，清理仍可识别且属于该 request 的 lifecycle，不 crash、retry、
  伪造 CcRef 或调用 final CC；`async_m:handle_info/3` 返回
  `{legacy_outcome, Outcome, CleanState}`，其中 `Outcome` 是原本要交给 continuation 的 reply、timeout、
  DOWN 或 message input，由 receive-loop 调用方自行处理。

分类只允许确认“无法进入已 purge 的旧代码”；callback 已经进入后发生的 ordinary exception 或 nested
`undef` 不得被吞掉或改写成 legacy outcome。

## 5. 主动检查和立即失败

用户可以在 `code_change/3` 中调用，也可以在进程自己的维护 API 中主动调用。核心 selector
应是函数 matcher，而不只支持 literal map。

已冻结接口：

```erlang
async_m:inspect_sf_callbacks(MatchFun, Offset, State) ->
    {ok, [Summary]} | {error, MatcherError}.

async_m:fail_sf_callbacks(MatchFun, Reason, Offset, State) ->
    {ok, [FailResult], NewState}
  | {error, MatcherError, State}.
```

Matcher 可以检查公开 metadata：

```erlang
#{
    request_mref => RequestMRef,
    sf_cc_mref => CcRef | undefined,
    protected => boolean(),
    continuation_kind => legacy_fun | sf_frame | unknown,
    frames => [#{
        module => Module,
        vsn => Vsn,
        protocol => Protocol,
        block_path => BlockPath,
        step_name => StepName | undefined,
        async_id => AsyncId
    }]
}.
```

约束：

- matcher 不接收 Env、完整 State、CC context、timer 或内部 callback value；
- matcher 只在本次调用中同步执行，不持久化 fun；
- 所有 matcher 在任何 State mutation 前运行；
- matcher exception 或非 boolean 返回使 State 不变；
- 选中 protected callback 后，runtime 按 CcRef 去重并以
  `{error, {sf_aborted, Reason}}` 完成；选中 unprotected old tuple 返回 `unprotected` 且不删除；
- CC 已被消费时不再通知；
- commit 按 CcRef term order；单个 CC crash 记录 `cc_failed` 并继续其他已 preflight CcRefs；
- selector convenience map 可以作为 matcher builder，但不是核心协议。

对普通 legacy `do`，runtime 通常没有稳定 BlockPath/StepName；除非创建 promise 时用户显式标注，
这些字段只能是 `undefined`。`sf_do` frame 可以提供完整字段。

## 6. Module `-vsn`

版本沿用 OTP module `-vsn`。`Vsn` 是任意 literal term，不能假定为 atom、字符串或 semver：

```erlang
-vsn("3.0.0").
-vsn(3).
-vsn({release, 3}).
```

OTP `code_change/3` 的方向形式为：

```erlang
Vsn
{down, Vsn}
```

`beam_lib:version/1` 会对非 list Vsn 使用 list wrapper；release handler 会对常见 singleton
non-string 形式解包后传给 `code_change/3`。实现必须统一 frame metadata、manifest、matcher 和
`code_change/3` 所看到的 effective Vsn，不能把 `beam_lib` 的外层 list误当成用户 Vsn。

官方参考：

- <https://www.erlang.org/doc/system/modules.html>
- <https://www.erlang.org/doc/apps/stdlib/beam_lib.html>
- <https://www.erlang.org/doc/apps/stdlib/gen_server.html#code_change-3>

没有显式 `-vsn` 时 OTP 使用编译后的 BEAM checksum。parse transform 阶段拿不到最终 checksum；
如何在 frame 中精确保存这个 effective Vsn 尚未定案。

## 7. `sf-migrate` 候选方向

### 7.1 用户提供的信息

迁移规则应允许用户提供：

- Environment 如何转换，可选；
- reply/Input 如何转换，可选；
- 旧 block 的编译期信息，可选；
- source/target Module、Vsn、BlockPath、StepName mapping；
- 无法迁移时是否立即终止的显式策略；默认始终存在不可恢复失败路径。

缺省转换的候选语义：

- 没有 Env migrator：复用实际 stored Env，再以 target manifest 检查 required captures；
- 没有 Input migrator：Input 原样交给 target continuation；
- 没有旧 block metadata：upgrade 可以匹配实际 stored frame，compiler 不提供旧端存在性和
  CaptureAbi 的静态证明；拼写或版本错误推迟到 runtime；
- 用户显式 mapping 表示其承担旧 Input 与 target continuation 的语义兼容责任。

### 7.2 Identity

named `sf_do` identity 仍使用：

```erlang
crypto:hash(
    sha256,
    term_to_binary({sf_named_async, Protocol, BlockPath, StepName})
).
```

Module 通过 frame MFA/owner metadata区分；Vsn 用于选择版本迁移规则，不加入当前 named
`AsyncId` hash。Block 必须使用完整 lexical BlockPath，不能只取最后一个 atom。

### 7.3 不持有旧 block 的 upgrade

upgrade runtime 可以：

1. 从实际 callback frame 读取 owner Module、Vsn、Protocol、BlockPath、StepName、AsyncId、Env；
2. 重新计算并验证 source identity；
3. 按 source endpoint/version 查 migration rule；
4. 从当前 owner module manifest 解析 target MFA 和 required captures；
5. 转换 Env；
6. reply 到达后转换 Input；
7. 执行 target frame；
8. 任一步不能处理时消费 promise 的最终 `CcRef`。

旧 block metadata 在该路径只提供更早的 compiler diagnostics，而不是运行必要条件。

### 7.4 外部模块独立升级

callbacks field 属于持有 State 的进程，不属于定义 frame 的模块。模块 B 升级不会自动调用
持有 B frame 的模块 A 进程的 `code_change/3`，B 也不能直接修改 A 的私有 State。

因此 correctness 不能只建立在 eager `code_change` migration 上：

- eager path：持有进程在自己的 `code_change/3` 或维护 API 中主动 migrate/fail；
- lazy path：reply 恢复每个 frame 前，按 frame owner Module 的当前 manifest/catalog 尝试迁移；
- migration 只处理 owner module匹配的 frame；其他模块 frame bit-for-bit 保留；
- continuation chain 可以依次经过不同模块，每个节点在真正执行前独立 resolve；
- lazy migration仍失败时，跳过旧 continuation并调用 promise最终 CC。

### 7.5 Downgrade

仅凭旧 BlockPath/StepName 可以计算 deterministic AsyncId/generated function atom，但不能证明目标
旧版本实际包含该 endpoint，也不能在旧代码加载前证明 target CaptureAbi/required context。

候选边界：

- upgrade：historical block/metadata 可选；
- strict downgrade frame migration：historical target manifest metadata 必需；
- metadata缺失时：只能把匹配 promise terminal-fail 到最终 CC，不能声称已安全构造旧 frame；
- 若要求外部模块 downgrade 前主动处理其他进程持有的 callbacks，还需要发布协调/registry；
  lazy target code无法一般性解决“旧代码预知未来新 frame”的问题。

## 8. 与当前正式文档/实现的主要冲突

### 8.1 `sf_cc` 当前嵌在 continuation chain

当前 `SfContinuation` 以完整 `sf_cc` 结尾，structural failure 通过递归遍历 downstream 找 CC。
新方向增加独立 `CcRef`，目的是即使 anonymous continuation 或 continuation shape 不可用，也仍能
到达最终 CC。候选方向是 action callback 通过新增的 `#callback.sf_cc_mref` 到达独立
的最终 CC entry。stable chain 尾部是否同时保留 CC data 仍需 final spec 冻结，但不应再
引入 callback envelope。

当前 `sf_cc` runtime contract 只使用 `{Module, Function, 2}`，并调用
`Module:Function(Outcome, Context)`。已冻结接口改为保存用户写出的 external-fun term，
接受 `fun Module:Function/2` 和 `fun Module:Function/3`；后者还接收 CC 调用时的
State。这同时是 CC 表示和调用协议的扩展，需要与 `CcRef` store encoding 一起冻结，
不能只修改 macro。

### 8.2 当前 frame/manifest 没有 Vsn

当前 `sf_frame` 只有 protocol、BlockPath、StepName、AsyncId、MFA 和 Env；manifest 也没有 module
effective Vsn。版本 matcher和版本 migration需要新增一致的 Vsn metadata。

### 8.3 当前 `sf-migrate` 强制保留旧 block

当前 proposal 使用：

```erlang
sf_migrate(OldDoBlock, Spec)
```

并通过完整 historical block分析旧 AsyncId/CaptureAbi。本文候选方向要求旧 block metadata 对
upgrade 可选，因此 declaration syntax、compiler分析和 manifest schema都需要重写。

### 8.4 当前 migration 隔离 legacy callbacks

当前 spec 要求 legacy `#callback{}` bit-for-bit 不变；本文改为允许最小 record 扩展，
增加 `sf_cc_mref` 字段，使新创建的 anonymous continuation promise 也能主动失败。需要区分：

- 上线前已经存在的三元 record tuple，兼容读取为 `sf_cc_mref = undefined`；
- 新 runtime 创建的四元 record tuple，`cc` 仍是原 anonymous fun 或 frame data，另存
  `sf_cc_mref = CcRef`。

### 8.5 当前显式 migration 只在 API 调用时执行

当前 spec 明确禁止未调用 `migrate_sf_callbacks/4` 时运行显式 mapping。本文为了处理外部模块
独立升级，引入 resume-time lazy migration。这是正式行为变更，不是实现细节。

### 8.6 当前没有 Input migration

当前方案只迁移 captured Env。本文增加可选 reply/Input transformation；需要新的调用协议、异常
处理、版本链组合和测试。

## 9. 已冻结的 sf-cc 决策与尚未冻结的 sf-migrate 缺口

9.1 至 9.5、9.9 已由 `sf-cc` change 冻结；其余问题未解决前，不应直接实现完整 `sf-migrate`。

### 9.1 CcRef 创建边界（已冻结）

升级后每次 public runner invocation 都创建新 `CcRef`：

- `wait/1`、`wait_t/2` 使用 library-owned completion；
- `run_cc/2`、`run_with_cc/4`、`exec_cc/4`、`run/4` 使用 legacy final adapter；
- `run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5` 使用 stable `sf_cc` descriptor；
- 同一 invocation 内 resume 的 continuation 和后续 action 继承当前 CcRef；
- nested code 显式再次调用任一 public runner 时创建新的 CcRef；
- final CC 返回的裸 async effect 不复用已消费 CcRef；effect 内显式 runner invocation 按上一条创建新 CcRef。

唯一例外是升级前已经 pending 的旧三元 tuple；它没有 CcRef，runtime 不补造。

### 9.2 Store encoding 和路由（已冻结）

`sf-cc` 已冻结：

- `sf_cc_ref` 使用 protocol `1` map，并持有 final descriptor；
- `#callback.sf_cc_mref` 由 shared record/decoder 读取，兼容 old/new tuple；
- `callback_exists/1` 只统计 action callback，CcRef entry 不是 reply target；
- stable continuation尾部只保存 `#{kind => sf_cc_ref, protocol => 1, mref => CcRef}`；
- message path 不消费 action callback 或 CcRef；
- completion 依次 preflight、take CcRef、清理 owned action lifecycle，再调用 final descriptor。

### 9.3 内部并行的清理边界（已冻结）

一个 promise、一个最终 Outcome、一个 CcRef。同一 CcRef 下物化的内部 callback entries 属于该 promise
的 final lifecycle，completion 清理它们及其 timer/monitor/无引用 accumulator，并只调用 final CC 一次。
这不把 CcRef 定义成多个独立业务 promises 的 operation-group identity；独立 runner invocation 有独立 CcRef。

### 9.4 已存在 legacy record 的上线兼容（已冻结）

保留三元 tuple reader，不重启或伪造 `CcRef`。continuation 可调用时按旧路径运行；若因 purge 确认
不可调用，则清理 request lifecycle，并由 `handle_info/3` 返回原 action input 的 tagged legacy outcome。
所有 decode/router/inspection/callback existence 入口必须共享 old/new tuple semantics。

### 9.5 Selector 最终 API（已冻结）

matcher 是 callback-level function，每个 request callback 调用一次；summary 包含 current-to-downstream frame
list。matcher crash/非 boolean 在 mutation 前失败，active fail 按 CcRef 去重并返回 per-CcRef status；
legacy callback 以空 frames 和 `protected = false`/`sf_cc_mref = undefined` 表示。

### 9.6 Vsn 缺省和多 attribute 标准化

需要决定：

- migration-aware module是否必须显式 `-vsn`；
- 无显式 Vsn 时如何取得最终 BEAM checksum；
- 多个 `-vsn` attribute 是否支持；
- frame metadata与 OTP release handler normalization 如何保持完全相同。

### 9.7 `sf_migrate` source syntax

需要冻结：

- 无旧 block declaration 的正式语法；
- source/target endpoint shape；
- version anchors；
- optional historical metadata 的提供方式；
- Env/Input migrator继承和方向规则；
- current manifest/migration catalog schema；
- 多次升级未收到 reply 时，直接迁移还是组合版本链。

### 9.8 Input migrator 合同

至少需要决定：

- 输入是 raw Input 还是包含 source/target metadata 的 context map；
- 返回 `{ok, TargetInput} | {error, Reason}` 还是允许直接值；
- timeout、DOWN、message是否进入转换；
- converter crash是否总是 terminal；
- eager code change只能保存 adapter、不能转换尚未到达的 Input时，adapter如何持久化。

### 9.9 Code-change 调用 final CC 的事务边界（已冻结）

基础事务顺序已确定：preflight 成功后，runtime 先删除 action callback、相关资源和
CcRef，再以 CleanState 调用 final CC。CC crash 不恢复 CcRef、不重试 promise，code change
也不因已发生的 CC 外部副作用而 rollback。

批量主动失败先对全部 matcher 做无副作用 preflight，再按 CcRef term order commit。一个 CC crash 记录
`cc_failed`，保持该 CcRef consumed，并继续处理其余已 preflight CcRefs。

### 9.10 Downgrade 和外部进程协调

需要确认 strict downgrade 是否正式要求：

- historical manifest artifact；
- pending callback registry；
- release coordinator广播 owner processes；
- 或只承诺无法迁移时 terminal-fail，不承诺所有其他进程在 purge 前完成 eager migration。

## 10. 建议实施顺序

### Slice 1：最终完成基础设施

- 按已冻结合同实现 CcRef创建边界和 store encoding；
- 实现独立 `run_sf_cc/3` 和 `run_with_sf_cc/5` macros：前者静态只接受 external callback `/2`，
  后者接受 external callback `/2` 或 `/3`；
- 让普通 `async_m:run_sf_cc/3` 和 `async_m:run_with_sf_cc/5` 保存 external-fun term、runner mode
  和预存 Context，
  不拆成 module/function/arity；
- final invoker 在 runtime 按 mode/arity 分派：async_r_t mode 执行 `/2` 返回的 effect；
  direct-State mode 忽略 `/2` 返回值，或把 `/3` 返回值作为 NewState；
- 为新 `#callback{}` 填充 `sf_cc_mref`，保持 `cc` 内容不变；
- 让 wait 和 existing ordinary runners 通过 library/legacy final descriptor 使用同一 CcRef core；
- 实现唯一 `complete_sf_cc/3`；
- 保持旧 `#callback{}` reader兼容；
- 添加 normal completion、failure、late reply、exactly-once测试。

### Slice 2：Legacy anonymous callback终止

- 新创建的 anonymous continuation callback 填充 `sf_cc_mref`，但不包装或改写 fun；
- 增加主动 fail matcher；
- 增加 resume-time invalid-fun兜底；
- 实现已冻结的旧三元 record tuple 兼容 reader 与 `handle_info/3` tagged legacy outcome；
- 添加 appup/purge和人工终止测试。

### Slice 3：Frame metadata和跨模块 resolution

- 为 frame/manifest增加 effective module Vsn；
- matcher暴露 Module/Vsn/BlockPath/StepName；
- continuation逐 frame owner module验证；
- 添加同一 callbacks store多模块和跨模块 nested chain测试。

### Slice 4：无旧 block的 upgrade migration

- 冻结 endpoint DSL和migration catalog；
- 实现 optional Env/Input migration；
- 实现 code-change eager path和resume-time lazy path；
- migration失败统一消费最终 CcRef；
- 添加 external module独立升级测试。

### Slice 5：Historical metadata和 downgrade

- 冻结 historical manifest artifact；
- 实现 target-old frame preflight/construction；
- 明确跨进程协调或 terminal-only边界；
- 添加真实 appup/appdown ordering测试。

## 11. 当前可实施性判定

- 可以立即将本文作为讨论预览落盘。
- `sf-cc` 与 `sf-do` 已可按各自 OpenSpec change 实现；`sf-migrate` 在剩余缺口解决前不应标为 final。
- 不应直接实现完整 `sf-migrate`。
- 最适合先实现的是 Slice 1；它为 legacy failure、主动终止、lazy migration和 downgrade failure
  提供统一的最终完成基础设施。

## 12. 必要验收场景预览

最终正式 spec 至少应覆盖：

- `run_sf_cc/3` macro 只接受 external fun `/2`，`run_with_sf_cc/5` macro 接受 external fun `/2` 和 `/3`；
- 两个 macro 都拒绝 local fun、anonymous fun 及 mode 不支持的 arity；
- macro 展开后的普通 `async_m:run_sf_cc/3` 或 `async_m:run_with_sf_cc/5` 收到与 source 中相同的
  external-fun term，而非 MFA tuple；
- immediate promise正常调用用户 `sf_cc` 一次；
- wait 和 ordinary runners 在保持旧正常结果的同时创建 CcRef，且不要求用户 callback 是 external fun；
- async_r_t runner 的 `/2` CC 返回 `async_r_t`，并可从 effect 内获取 State；
- `run_with_sf_cc` 的 `/2` CC 收到 `(Outcome, PresavedContext)` 且返回值被忽略；
- `run_with_sf_cc` 的 `/3` CC 收到 `(Outcome, PresavedContext, CleanState)` 并返回
  NewState；CleanState 是完成时已清理 callbacks/CcRef 的值，而非 `run_with_sf_cc/5`
  注册时的值；
- legacy anonymous continuation正常恢复并调用相同 CC；
- protected anonymous continuation 因 purge 不可用时跳过 continuation并调用其 CcRef final CC；
- pre-upgrade old tuple 因 purge 不可用时，`handle_info/3` 返回原 action input 的
  `{legacy_outcome, Outcome, CleanState}`，且不调用 final CC；
- ordinary callback exception 或 nested `undef` 不被误判为 old-code unavailable；
- stable frame structural/migration failure调用相同 CC；
- 人工 matcher按 Module/Vsn/BlockPath/StepName筛选；
- matcher crash不修改 State；
- CcRef消费后迟到 reply不重复 completion；
- message不错误消费最终 CcRef；
- 内部并行失败不会重复调用最终 CC，也不会泄漏 timer/monitor；
- 同一 callbacks store包含不同模块定义的 frames，迁移只处理目标 owner module；
- 外部 frame owner module独立升级、持有进程未执行 code change时，reply-time仍能 migrate或终止；
- 无 historical block的 upgrade；
- Env identity/转换和 Input identity/转换；
- 多个 source Vsn；
- strict downgrade metadata缺失时 fail closed或 terminal-fail，行为与最终合同一致；
- legacy old record 按 `sf_cc_mref = undefined` 兼容执行；purged/unavailable 时返回 tagged
  legacy outcome，不获得 final CC fallback。
