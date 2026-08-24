# Promise 重构可选方案

## 范围

本文只讨论当前项目中 Promise 的内部表示、组合和消费端运行时。

Task 预计独立实现，并通过类似以下接口生成 Promise：

```erlang
async_task:result(Task) -> Promise.
```

Promise 不负责 Task 的 PID、owner、父子关系、取消、调度或资源生命周期。

## 硬约束：Producer 无感

Producer 不得因为 Promise 重构而修改业务逻辑或接入专用的 Promise
producer API。它继续使用现有 OTP/request-reply 方式工作：

```erlang
{message, Ref, Progress}
{Ref, FinalReply}
{'DOWN', Ref, _, _, Reason}
```

普通 final reply 继续使用现有 reply 机制。只有主动发送 progress 时，Producer
才需要调用现已存在的 `async:message/2`。

所有 Promise 知识均位于消费端：

```text
Producer（不变）
    |
    | 现有消息协议
    v
Source Descriptor
    |
    v
Consumer Registry / Router
    |
    v
Promise 表示与组合层
```

Resolver、Publisher、专用 reply envelope 或 Producer 持有的 Promise handle 都不能成为
默认核心模型。

## 当前架构

`async_m` 是 `async_t` 在 `identity` Monad 上的主要入口。当前 Promise 的核心
表示本质上是 CPS：

```erlang
AsyncT A = (Reply A -> AsyncR R) -> AsyncR R.
```

实际实现组合了：

```text
ReplyT
  ContT
    StateT
      ReaderT
        ReaderT
          InnerMonad
```

这些层同时承担：

- 动态顺序组合；
- progress/final/error reply 传播；
- continuation；
- 应用 state 和 callback-local state；
- callback registry；
- 阻塞等待和 OTP `handle_info` 接入；
- 有限并发聚合。

重构不要默认改变这些已有行为。

## 共同的消费端底座

### Source Descriptor

Source Descriptor 把“如何发起或观察现有异步源”与 Promise 组合分离：

```erlang
-record(async_source, {
    open,
    decode,
    cleanup
}).
```

概念接口：

```erlang
open() ->
    {pending, CorrelationKey}
  | {immediate, Reply}.

decode(CorrelationKey, Info) ->
    no_match
  | {progress, Progress}
  | {settled, Outcome}.

cleanup(CorrelationKey) -> ok.
```

默认 reference source 可以完全兼容当前 `promise(Action)` 行为。Source Descriptor
也能为未来 Task result、socket、port 或其他消息协议提供消费端 adapter，
无需修改 Producer。

### Consumer Registry / Router

```erlang
-record(subscription, {
    key,
    decode,
    continuation,
    progress_handler,
    cleanup
}).

-record(async_registry, {
    subscriptions = #{}
}).
```

Registry 按 correlation key 路由收到的 OTP 消息。final reply 移除 subscription，
progress 保留 subscription。阻塞 `wait` 和嵌入式 `handle_info` 应复用相同的
路由语义。

Source Descriptor 和 Registry 不是必须与单一 Monad 二选一的方案，而是保证
Producer 无感的共同边界。

## 可选方案

### A. 单一 Fused CPS Monad

把现有 transformer stack 机械融合成一个专用 `async_m` Monad。将 `Identity`
去掉，并把 State 与两层 Reader 合并为 context：

```erlang
-record(async_context, {
    state,
    local_ref,
    callbacks_get,
    callbacks_set
}).

-type runtime(A) ::
    fun((async_context()) -> {A, async_context()}).

-type async(A, R) ::
    fun((fun((reply(A)) -> runtime(R))) -> runtime(R)).
```

`return`、`bind`、`fail`、`catch_error`、state/local 操作和 Promise suspension 都直接在这个
类型上实现。

优点：

- 与当前 CPS 实现最接近；
- 可以保留 ErLando `do` 和公开 API；
- 动态顺序依赖仍然自然；
- 不需要新的 interpreter；
- 可以通过新旧实现对照测试验证；
- 迁移成本和风险最低。

局限：

- Promise 是闭包，结构不可观察；
- continuation 深链不容易诊断；
- `all`/`race` 等需要专用组合器实现。

如果目标是在保持行为的前提下移除 transformer，这是基准方案。

### B. Promise AST + Interpreter

Promise 表示成消费端数据：

```erlang
-type promise(A) ::
    {source, async_source()}
  | {resolved, A}
  | {rejected, term()}
  | {map, promise(term()), fun((term()) -> A)}
  | {then, promise(term()), fun((term()) -> promise(A))}
  | {recover, promise(A), fun((term()) -> promise(A))}
  | {all, [promise(term())], map()}
  | {race, [promise(A)], map()}.
```

AST 叶节点是 Source Descriptor，所以 Producer 仍然无感。

相对 Fused Monad 的优势：

- `all`/`race`/`timeout` 是显式节点；
- 可打印和描述 Promise pipeline；
- 可为节点分配 tracing ID 并统计耗时；
- 可替换 interpreter 做测试；
- 可合并连续 `map` 或执行局部优化。

代价：

- 引入 interpreter 复杂度和额外 dispatch；
- `then` 仍然包含函数，不能完整序列化；
- 对当前功能可能过度设计。

### C. Monad API + Continuation Frame Runner

对外保留 Monad/`do` 组合，内部不使用嵌套 CPS 闭包，而是显式 frame：

```erlang
-record(promise, {
    source,
    frames = []
}).

-type frame() ::
    {map, fun((term()) -> term())}
  | {then, fun((term()) -> promise())}
  | {recover, fun((term()) -> promise())}
  | {finally, fun(() -> any())}.
```

Runner 逐个解释 frame：

```erlang
resume({fulfilled, Value}, [{map, F} | Rest], Run) ->
    resume({fulfilled, F(Value)}, Rest, Run);
resume({fulfilled, Value}, [{then, F} | Rest], Run) ->
    open_promise(F(Value), Rest, Run).
```

相对直接 Fused CPS 的优势：

- 深同步链可使用 trampoline；
- 当前 continuation 位置可检查；
- 连续 `map` 可以融合；
- `recover`/`finally` 的位置显式；
- 运行中 Promise 的诊断更容易。

代价是需要实现小型 frame VM。这是最值得在 Fused Monad 之后按需演进的
方案，公开 API 无需改变。

### D. Observer Combinators

Promise 表示为消费端注册动作：

```erlang
-type promise(A) ::
    fun((observer(A), async_registry()) -> async_registry()).
```

progress 和 settled 由 observer 的不同分支处理。可选 Monad adapter 可继续提供
`do` 语法。

相对 Fused Monad 的优势：

- progress 和 settled 天然分离；
- subscribe/unsubscribe 语义容易表达；
- 更容易扩展到多 consumer。

局限：

- 顺序组合本质上仍然是 continuation；
- 不提供 Monad adapter 时容易退化为 callback nesting；
- 对当前单 consumer 模型收益有限。

### E. Consumer-side Shared Promise Hub

Hub process 由消费端发起现有请求，Producer 仍然无感。Hub 缓存 settled
outcome 并管理多个 observer。

优势：

- 多 consumer；
- settled 结果缓存；
- late subscriber；
- 跨进程共享；
- 底层 request 只执行一次。

代价：

- 额外进程和消息跳转；
- 需要定义 Hub 生命周期和 progress 重放语义。

它应当是显式的 `share(Promise)` 组合器，不应成为默认 Promise 表示。

### F. Consumer-side DAG

DAG 的叶节点仍是 Source Descriptor，所以不影响 Producer。DAG scheduler 在消费端
管理静态依赖、拓扑调度和并发限制。

优势：

- 静态依赖可自动并行；
- concurrency limit 由统一 scheduler 管理；
- 可视化和节点结果缓存更自然。

局限：

- 不能自然替代动态 bind；
- 当下一步的结构取决于上一步结果时，需要 continuation 或动态扩图；
- 适合作为 `map_promises` 之上的高级组合层，不适合作为基础 Promise。

### G. Progress Stream 视图

可以在消费端将 Promise 拆成：

```text
Progress Stream + Final Outcome
```

这样可以对 progress 提供 `map`/`filter`/`throttle`/`buffer` 等操作，仍然
不影响 Producer。但它会引入背压、buffer 和订阅生命周期问题，因此应作为
Promise 的可选视图，而不是默认底层表示。

## 方案对比

| 方案 | 保留 `do` | 迁移成本 | 可观察性 | 多 consumer | 静态并行分析 | 主要用途 |
| --- | --- | --- | --- | --- | --- | --- |
| Fused CPS Monad | 是 | 低 | 低 | 弱 | 无 | 等价替换当前实现 |
| Promise AST | 可以 | 高 | 高 | 中 | 中 | tracing、解释和优化 |
| Continuation Frames | 是 | 中 | 中 | 弱 | 无 | 栈安全和运行时诊断 |
| Observer | 可选 | 中 | 中 | 强 | 无 | 订阅和多 consumer |
| Shared Hub | 与表层无关 | 中 | 高 | 强 | 无 | 共享 Future |
| DAG | 与表层无关 | 高 | 高 | 中 | 强 | 批处理和依赖调度 |

## 推荐路线

### 第一阶段：等价融合

```text
Producer               不变
    |
Source Descriptor      薄适配层
    |
Registry / Router      消费端 correlation
    |
Fused async Monad      保留当前公开组合语义
```

1. 新建单一 Fused CPS Monad；
2. 保留 `async_m` 公开 API 和 ErLando `do`；
3. 保留现有 reference 和 reply 协议；
4. 使用新旧实现对照测试验证行为等价；
5. `async_t` 可先保留为 generic inner-Monad 兼容层。

### 第二阶段：按实际问题演进

- 若出现深链或诊断问题，将内部 continuation 换成 frame runner；
- 若需要多 consumer 或 late subscriber，增加显式 `share(Promise)` Hub；
- 若需要 tracing/可视化/静态优化，再评估 Promise AST；
- 若需要复杂批处理，在 Promise 上增加 DAG scheduler；
- 若 progress 变成流数据，提供可选 Progress Stream 视图。

## 结论

当前的首选是：

> Source Descriptor + Consumer Registry + 单一 Fused CPS Monad。

这一方案保留 Producer 无感、现有 Promise 协议和公开组合方式，只替换
Promise 的 transformer 实现。AST、frame VM、shared Hub、DAG 和 progress stream
均是针对新需求的可选演进，不应在没有实际需求时扩大第一阶段的
重构范围。
