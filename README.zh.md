[![CI](https://github.com/slepher/async/actions/workflows/ci.yml/badge.svg?branch=master&event=push)](https://github.com/slepher/async/actions/workflows/ci.yml?query=branch%3Amaster)

[![CI](https://github.com/slepher/async/actions/workflows/release.yml/badge.svg?branch=0.6.8&event=push)](https://github.com/slepher/async/actions/workflows/release.yml?query=branch%3A0.6.8)

# async

[English](README.md) | [简体中文](README.zh.md)

`async` 是一个 Erlang 异步请求/回复组合库。它把异步操作表示成可组合的
Monad 值：一个 promise 可以依次产生零条或多条进度消息，最后必须产生且只
产生一个 final reply。工作流可以顺序组合、转换、接入 OTP 进程，也可以在
限制活动任务数的情况下并行聚合。

主要入口是 `async_m`。它是通用 Monad Transformer `async_t` 在
`identity` Monad 上的特化版本。

## 功能概览

- 使用 ErLando `do` 记法组合异步操作。
- 接收多条 `{message, Message}`，最后接收一个 final reply。
- 在 Monad 链中传播 `{error, Reason}`。
- 支持单 promise 超时和整体等待超时。
- 并行聚合 promise 列表或 map，并可限制并发数。
- 在 continuation 中携带应用状态和回调局部状态。
- 将未完成 promise 接入 `gen_server` 风格的 `handle_info/2`。
- 为 `gen_server`、`gen_fsm`、RPC 和原始监控请求创建 promise。
- 在 `async_m` 不够通用时，使用带自定义内层 Monad 的 `async_t`。

## 状态与兼容性

当前仓库中的应用版本是 `0.6.0`。

项目目前没有声明最低 Erlang/OTP 版本。监督树使用 map child spec，并以普通
`one_for_one` supervisor 管理静态和动态子进程。使用方应在自己的目标 OTP
版本上编译并运行测试。

## 安装

在 `rebar.config` 中加入 Git 依赖：

```erlang
{deps, [
    {async, {git, "https://github.com/slepher/async.git", {tag, "0.6.0"}}}
]}.
```

然后编译：

```shell
rebar3 compile
```

使用受监督的 worker 或 channel 时，需要启动应用：

```erlang
ok = case async:start() of
         {ok, _Apps} -> ok;
         {error, {already_started, async}} -> ok
     end.
```

在 OTP release 中，通常更适合把 `async` 加入使用方应用的
`applications` 列表，让 release 自动启动它。

## 快速开始

引入 ErLando 的 `do` 记法并构造工作流：

```erlang
-module(example).
-export([run/0]).

-include_lib("erlando/include/do.hrl").

run() ->
    Workflow =
        do([async_m ||
               async_m:promise_sleep(25),
               return(done)
           ]),
    async_m:wait(Workflow).
```

结果为：

```erlang
{ok, done}
```

`async_m:return/1` 创建普通 Monad 成功值；`async_m:pure_return/1` 直接注入
一个已经成形的 reply，不额外添加 `{ok, ...}`：

```erlang
async_m:wait(async_m:return(value)).
%% => {ok, value}

async_m:wait(async_m:pure_return(value)).
%% => value
```

`pure_return/1` 主要用于实现协议级组合器，或者明确需要未包装 reply 的场景。

## Promise 与 reply 协议

异步源由类似 reference 的值标识。支持的标识类型是 reference、integer 和
binary。发送到等待进程的回复格式为：

```erlang
{message, Ref, Message}  %% 零条或多条非最终消息
{Ref, FinalReply}        %% 恰好一个最终回复
{'DOWN', Ref, _, _, Reason}
```

`async_t` 向用户回调暴露的 reply 格式为：

```erlang
{message, Message}
{ok, Value}
{error, Reason}
Value
```

核心不变量是：

> 一个 promise 可以产生任意数量的 message，但必须产生且只产生一个
> final reply。

final reply 会移除已注册 callback；message 不会移除 callback，因此同一个
promise 可以继续发送后续事件。

### 创建 promise

包装已有请求 reference：

```erlang
Ref = async_gen_server:call(Server, Request),
Promise = async_m:promise(Ref).
```

也可以推迟到 promise 真正启动时再创建 reference：

```erlang
Promise =
    async_m:promise(
      fun() ->
          async_gen_server:call(Server, Request)
      end).
```

如果零参数 action 返回 reference、integer 或 binary，promise 会等待与该
标识关联的回复。返回其他值时，该值会被视为立即得到的 final value。

### 发送进度消息

服务端可以通过标准 `From` 值发送进度，最后给出正常回复：

```erlang
handle_call(work, From, State) ->
    async:message(From, started),
    async:message(From, halfway),
    {reply, {ok, finished}, State}.
```

使用 `wait_t/2` 消费消息：

```erlang
Result =
    async_m:wait_t(
      async_gen_server:promise_call(Server, work),
      #{callback =>
            fun({message, Message}) ->
                    io:format("progress: ~p~n", [Message]),
                    ok;
               (FinalReply) ->
                    FinalReply
            end}).
```

## 顺序组合

Promise 支持标准 ErLando Monad 操作：

```erlang
Workflow =
    do([async_m ||
           User <- async_gen_server:promise_call(user_server, get_user),
           Orders <- async_gen_server:promise_call(
                       order_server, {orders_for, User}),
           return({User, Orders})
       ]).
```

`{error, Reason}` 会短路后续 Monad 步骤：

```erlang
Workflow =
    do([async_m ||
           _ <- async_m:fail(invalid_request),
           return(unreachable)
       ]),

{error, invalid_request} = async_m:wait(Workflow).
```

使用 `monad_error:catch_error/2` 恢复：

```erlang
Recovered =
    monad_error:catch_error(
      async_m:fail(not_found),
      fun(not_found) -> async_m:return(default_value) end).
```

## 使用 `map_promises` 并行聚合

当每个输入 promise 都贡献一个 final result 时，推荐使用
`map_promises` 进行聚合。

### 列表

列表结果保持输入位置：

```erlang
Delayed =
    fun(Milliseconds, Value) ->
        do([async_m ||
               async_m:promise_sleep(Milliseconds),
               async_m:pure_return(Value)
           ])
    end,

Promises = [
    Delayed(30, first),
    Delayed(10, second),
    Delayed(20, third)
],

[first, second, third] =
    async_m:wait(async_m:map_promises(Promises)).
```

### Map

Map 结果保留 key：

```erlang
Promises = #{
    profile => async_gen_server:promise_call(profile_server, UserId),
    settings => async_gen_server:promise_call(settings_server, UserId)
},

#{profile := Profile, settings := Settings} =
    async_m:wait(async_m:map_promises(Promises)).
```

Map 中 promise 产生的 message 会带上对应 key：

```erlang
{message, {profile, Progress}}
```

列表版本内部使用数字 key，但在向调用方转发 message 前会移除该数字 key。

### 并发限制

通过 `limit` 限制活动工作数：

```erlang
Result =
    async_m:wait(
      async_m:map_promises(
        Promises,
        #{limit => 4})).
```

`limit => 0` 是默认值，表示不限制。正数限制采用内部
Working/Pending/Completed 生命周期：

1. 最多 `limit` 个 key 进入 Working。
2. 其余 key 留在 Pending。
3. Working promise 可以产生多条 message。
4. final reply 对应的 callback 完成后，结果才进入 Completed。
5. 释放出的 Working 槽位启动一个 Pending promise。
6. 所有初始工作链都耗尽后，聚合才返回。

final-reply callback 属于工作单元。如果自定义 callback 是异步的，它完成前
会继续占用 Working 槽位；不同 Working 链仍会并行执行。

### 自定义累计

Map 版本接受以下选项：

```erlang
#{
    limit => non_neg_integer(),
    acc0 => InitialAccumulator,
    cc => fun(Key, Reply) -> AsyncT end
}
```

默认 callback：

- 把 `{message, Message}` 转发为 `{message, {Key, Message}}`；
- 以 `Key` 为键把 final reply 保存到 map。

自定义 callback 会同时收到 message 和 final reply，并且必须返回
`async_t`。它可以异步执行；`map_promises` 会等待它完成后再推进对应的
Working 链。

## 超时

### 单 promise 超时

```erlang
Promise = async_m:promise(RequestRef, 1000).
```

或者通过 transport adapter：

```erlang
Promise = async_gen_server:promise_call(Server, Request, 1000).
```

如果 timer 先触发，promise 会收到：

```erlang
{error, timeout}
```

### 整体等待超时

```erlang
Result = async_m:wait_t(Workflow, #{timeout => 5000}).
```

整体超时发生时，当前状态中仍注册的所有 callback 都会收到
`{error, timeout}`。这与创建每个 promise 时分别设置独立 timer 不同。

## 状态与局部状态

`async_m` 暴露两个相关的状态层：

- `get_state/0`、`put_state/1`、`modify_state/1` 操作完整运行状态，并保留
  已注册 callback。
- `get_local/0`、`put_local/1`、`modify_local/1` 操作与当前 continuation
  reference 关联的数据。

示例：

```erlang
Workflow =
    do([async_m ||
           async_m:put_local([]),
           Value <- async_m:promise(RequestRef),
           async_m:modify_local(fun(Values) -> [Value | Values] end),
           async_m:get_local()
       ]).
```

`local_ref/2` 和 `local/2` 提供作用域化的其他局部上下文访问。
更底层的 `find_ref/1`、`get_ref/2`、`put_ref/2`、`remove_ref/1` 直接操作
callback/reference 存储。

## 等待与 callback 形式

最简单的 runner 会阻塞，直到不存在已注册 callback：

```erlang
async_m:wait(Promise).
```

`wait_t/2` 接受：

```erlang
#{
    callback => Callback,
    cc => AsyncRContinuation,
    state => InitialState,
    offset => CallbackMapTupleIndex,
    timeout => MillisecondsOrInfinity
}
```

`callback` 可以是 0、1 或 2 元函数。二元 callback 的调用形式是
`Callback(Reply, State)`。如果它返回相同形状的 state，该值成为下一状态；
返回其他形状时，则以该值完成等待。

使用自定义 tuple/record state 时，`offset` 指向保存 callback map 的 tuple
元素：

```erlang
-record(state, {callbacks = #{}, events = []}).

async_m:wait_t(
  Promise,
  #{
      state => #state{},
      offset => #state.callbacks,
      callback =>
          fun({message, Event}, #state{events = Events} = State) ->
                  State#state{events = [Event | Events]};
             (Final, #state{events = Events}) ->
                  {Final, lists:reverse(Events)}
          end
  }).
```

## 接入 OTP 进程

`wait/1` 适合进程边界，但会阻塞调用进程。为了保持 OTP server 可响应，可用
`exec/4` 注册工作流，再把收到的消息交给 `handle_info/3`。

```erlang
-record(state, {callbacks = #{}, value}).

start_request(Promise, State) ->
    async_m:exec(
      Promise,
      fun({message, Progress}, S) ->
              io:format("~p~n", [Progress]),
              S;
         (Reply, S) ->
              S#state{value = Reply}
      end,
      #state.callbacks,
      State).

handle_info(Info, State) ->
    case async_m:handle_info(Info, #state.callbacks, State) of
        unhandled ->
            {noreply, State};
        NewState when is_record(NewState, state) ->
            {noreply, NewState}
    end.
```

## Transport adapter

### `gen_server`

```erlang
async_gen_server:call(Server, Request).
async_gen_server:promise_call(Server, Request).
async_gen_server:promise_call(Server, Request, Timeout).
```

### `gen_fsm`

```erlang
async_gen_fsm:promise_sync_send_event(Process, Event, Timeout).
async_gen_fsm:promise_sync_send_all_state_event(Process, Event, Timeout).
```

### RPC

`async_rpc:promise_call/4` 调用远程函数。如果远程函数返回 `async_t`，该辅助
函数会把它的 message 和 final reply 转发回来，并表示成本地 promise。

```erlang
async_rpc:promise_call(Node, Module, Function, Args).
```

### Channel

`async_channel` 提供带 pool-size 限制的受监督请求队列：

```erlang
{ok, _Pid} = async_channel:start(my_channel, 8),
Promise =
    async_gen_server:promise_channel_call(
      my_channel, Server, Request, Timeout).
```

### 原始监控调用

```erlang
Ref = async:call(Process, Label, Request),
Promise = async:promise_mref(Ref, Timeout).
```

`Process` 可以是 PID、本地注册名、`{global, Name}` 或 `{Name, Node}`。

## 架构

通用 Transformer 栈为：

```text
AsyncT s r m a
  = ReplyT Message Error
      (ContT r
        (AsyncRT s m)) a

AsyncRT s m a
  = StateT s
      (ReaderT Reference
        (ReaderT CallbacksGS m)) a
```

模块职责：

| 模块 | 职责 |
| --- | --- |
| `async_m` | 从 `async_t` 生成的主要 identity 特化 API |
| `async_t` | Promise 组合、reply 处理、等待、并行聚合 |
| `async_r_t` | 运行状态、当前局部 reference、callback 存储访问 |
| `reply_t` | message/final/error reply 语义 |
| `async` | 监控请求传输和进度消息发送 |
| `async_gen_server` | `gen_server` 请求 adapter |
| `async_gen_fsm` | `gen_fsm` 请求 adapter |
| `async_rpc` | 远程 promise 转发 |
| `async_channel` | 受监督的池化请求 channel |
| `async_worker` | 受监督的一次性 action worker |

除非明确需要选择其他内层 Monad，否则应使用 `async_m`。直接使用 `async_t`
时，需要显式传入 Monad 描述符：

```erlang
AT = async_t:new(identity),
Promise = async_t:promise(RequestRef, AT),
ResultMonad = async_t:wait(Promise, AT),
Result = identity:run(ResultMonad).
```

## 高级组合器与不变量

- `lift_reply/1` 把 message 和 final reply 都提升为更高 reply 层中的值。
- `lift_final_reply/1` 只把 final reply 提升为值；message 继续向外传播。
- `with_message/2`、`with_final/2`、`with_all/2` 在不同 reply 边界附加处理器。
- `handle_message/2` 使用 callback/continuation 消费 message。
- `pass/0` 有意从运行时层返回，不产生 final `async_t` reply。
- `par/1` 是底层接口：只能有一个分支产生 final reply，其他分支必须发送
  message 或使用 `pass/0`。
- `progn_par/1` 遵守该底层约束，并返回最后一个分支的值。
- 普通的全结果聚合应优先使用 `map_promises/1,2`。

违反“单 final reply”不变量可能导致 continuation 被调用多次、内部计数未清理，
或聚合过早完成。

## 开发

```shell
rebar3 compile
rebar3 ct
rebar3 xref
rebar3 dialyzer
```

测试套件覆盖 Transformer 状态、promise 链、错误、消息、超时、异步累计、
有限并发和历史 API 版本。

如需在多个 Erlang/OTP 版本上进行 Docker 兼容性测试：

```powershell
.\ci_scripts\sync_ci.ps1
.\ci_scripts\build.ps1
.\ci_scripts\run.ps1 -NoView
```

`ci_scripts/sync_ci.ps1` 会先执行 `rebar3 get-deps`，再通过
`_build/default` 解析 Rebar3 实际选中的 `astranaut` 依赖：远端依赖来自
`lib/astranaut`，本地 checkout 则通过 `checkouts/astranaut/src` 的链接
反查源码根目录，因此可以保留尚未提交的 checkout 修改。本仓库只维护
`ci_scripts/ci-env.conf.example`，本机覆盖配置位于被忽略的
`ci_scripts/ci-env.conf`；同步脚本不会替换这些项目配置。
上游 CI 文件集合发生变化后重新运行同步脚本即可。

Bash 环境使用 `bash ./ci_scripts/sync_ci.sh`。

在较新的 OTP 版本上，Dialyzer 可能报告与编译和 Common Test 结果无关的旧式
opaque/generated-code 警告。

## 许可证

BSD 3-Clause License，参见 [LICENSE](LICENSE)。
