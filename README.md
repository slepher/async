[![CI](https://github.com/slepher/async/actions/workflows/ci.yml/badge.svg?branch=master&event=push)](https://github.com/slepher/async/actions/workflows/ci.yml?query=branch%3Amaster)

[![CI](https://github.com/slepher/async/actions/workflows/release.yml/badge.svg?branch=0.6.11&event=push)](https://github.com/slepher/async/actions/workflows/release.yml?query=branch%3A0.6.11)

# async

[English](README.md) | [简体中文](README.zh.md)

`async` is an Erlang library for composing asynchronous request/reply workflows as
monadic values. A promise may emit zero or more progress messages and must
eventually emit one final reply. Workflows can be sequenced, transformed,
observed from an OTP process, or aggregated concurrently with a bounded number
of active jobs.

The public convenience API is `async_m`, an `identity`-specialized version of
the generic `async_t` monad transformer.

## Features

- Compose asynchronous operations with ErLando `do` notation.
- Consume multiple `{message, Message}` events followed by one final reply.
- Propagate `{error, Reason}` replies through monadic chains.
- Apply per-promise or whole-wait timeouts.
- Aggregate lists or maps of promises with optional concurrency limits.
- Carry application state and callback-local state through continuations.
- Integrate pending promises into `gen_server`-style `handle_info/2` loops.
- Create promises for `gen_server`, `gen_fsm`, RPC, and raw monitored calls.
- Use `async_t` over another inner monad when `async_m` is too specialized.

## Status and compatibility

The application version in this repository is `0.6.0`.

The project does not currently declare a minimum Erlang/OTP version. Its
supervision tree uses map child specifications and ordinary `one_for_one`
supervisors for both static and dynamic children. Applications should compile
and test it against their chosen OTP release.

## Installation

Add the Git repository to `rebar.config`:

```erlang
{deps, [
    {async, {git, "https://github.com/slepher/async.git", {tag, "0.6.0"}}}
]}.
```

Then compile:

```shell
rebar3 compile
```

Start the application explicitly when using its supervised workers or channels:

```erlang
ok = case async:start() of
         {ok, _Apps} -> ok;
         {error, {already_started, async}} -> ok
     end.
```

In an OTP release, adding `async` to the consuming application's
`applications` list is normally preferable.

## Quick start

Include ErLando's `do` notation and build a workflow:

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

The result is:

```erlang
{ok, done}
```

`async_m:return/1` creates a normal monadic success, while
`async_m:pure_return/1` injects an already-formed reply value without adding an
`{ok, ...}` wrapper:

```erlang
async_m:wait(async_m:return(value)).
%% => {ok, value}

async_m:wait(async_m:pure_return(value)).
%% => value
```

Use `pure_return/1` mainly when implementing protocol-level combinators or when
the unwrapped reply shape is intentional.

## Promise and reply protocol

An asynchronous source is identified by a reference-like value. The supported
identifiers are references, integers, and binaries. Replies delivered to the
waiting process use these shapes:

```erlang
{message, Ref, Message}  %% zero or more non-final messages
{Ref, FinalReply}        %% exactly one final reply
{'DOWN', Ref, _, _, Reason}
```

`async_t` exposes replies to user callbacks as:

```erlang
{message, Message}
{ok, Value}
{error, Reason}
Value
```

The central invariant is:

> A promise may emit any number of messages, but it must emit exactly one final
> reply.

Final replies remove the registered callback. Messages leave it registered so
the same promise can continue producing events.

### Creating a promise

Wrap an existing request reference:

```erlang
Ref = async_gen_server:call(Server, Request),
Promise = async_m:promise(Ref).
```

Or defer creation of the reference until the promise starts:

```erlang
Promise =
    async_m:promise(
      fun() ->
          async_gen_server:call(Server, Request)
      end).
```

If the zero-arity action returns a reference, integer, or binary, the promise
waits for replies associated with that identifier. Any other returned value is
treated as an immediate final value.

### Sending progress messages

A server can send progress through the standard `From` value and finish with a
normal reply:

```erlang
handle_call(work, From, State) ->
    async:message(From, started),
    async:message(From, halfway),
    {reply, {ok, finished}, State}.
```

Consume messages with `wait_t/2`:

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

## Sequential composition

Promises participate in the standard ErLando monad operations:

```erlang
Workflow =
    do([async_m ||
           User <- async_gen_server:promise_call(user_server, get_user),
           Orders <- async_gen_server:promise_call(
                       order_server, {orders_for, User}),
           return({User, Orders})
       ]).
```

An `{error, Reason}` reply short-circuits the remaining monadic steps:

```erlang
Workflow =
    do([async_m ||
           _ <- async_m:fail(invalid_request),
           return(unreachable)
       ]),

{error, invalid_request} = async_m:wait(Workflow).
```

Use `monad_error:catch_error/2` to recover:

```erlang
Recovered =
    monad_error:catch_error(
      async_m:fail(not_found),
      fun(not_found) -> async_m:return(default_value) end).
```

## Concurrent aggregation with `map_promises`

`map_promises` is the recommended aggregation primitive when every input
promise contributes one final result.

### Lists

List output preserves input position:

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

### Maps

Map output preserves keys:

```erlang
Promises = #{
    profile => async_gen_server:promise_call(profile_server, UserId),
    settings => async_gen_server:promise_call(settings_server, UserId)
},

#{profile := Profile, settings := Settings} =
    async_m:wait(async_m:map_promises(Promises)).
```

Messages from map values are tagged with their key:

```erlang
{message, {profile, Progress}}
```

The list form uses internal numeric keys and removes them before forwarding
messages to the caller.

### Concurrency limit

Pass `limit` to cap active work:

```erlang
Result =
    async_m:wait(
      async_m:map_promises(
        Promises,
        #{limit => 4})).
```

`limit => 0` is the default and means no limit. Positive limits use an internal
Working/Pending/Completed lifecycle:

1. Up to `limit` keys enter Working.
2. Remaining keys stay Pending.
3. A Working promise may emit multiple messages.
4. Its final-reply callback must finish before the result enters Completed.
5. The freed Working slot starts one Pending promise.
6. The aggregate returns only after every initial working chain has drained.

The final-reply callback is part of the work unit. If a custom callback is
asynchronous, its Working slot remains occupied until the callback finishes.
Different Working chains still run concurrently.

### Custom accumulation

The map form accepts:

```erlang
#{
    limit => non_neg_integer(),
    acc0 => InitialAccumulator,
    cc => fun(Key, Reply) -> AsyncT end
}
```

The default callback:

- forwards `{message, Message}` as `{message, {Key, Message}}`;
- stores final replies in a map under `Key`.

A custom callback is invoked for both messages and final replies and must return
an `async_t` value. It may be asynchronous; `map_promises` waits for it before
advancing that Working chain.

## Timeouts

### Per-promise timeout

```erlang
Promise = async_m:promise(RequestRef, 1000).
```

Or with a transport adapter:

```erlang
Promise = async_gen_server:promise_call(Server, Request, 1000).
```

If the timer wins, the promise receives:

```erlang
{error, timeout}
```

### Whole-wait timeout

```erlang
Result = async_m:wait_t(Workflow, #{timeout => 5000}).
```

When this timeout expires, all callbacks still registered in the current state
are driven with `{error, timeout}`. This differs from assigning an independent
timer when each promise is created.

## State and local state

`async_m` exposes two related state layers:

- `get_state/0`, `put_state/1`, `modify_state/1` operate on the complete runtime
  state while preserving registered callbacks.
- `get_local/0`, `put_local/1`, `modify_local/1` operate on data associated with
  the current continuation reference.

Example:

```erlang
Workflow =
    do([async_m ||
           async_m:put_local([]),
           Value <- async_m:promise(RequestRef),
           async_m:modify_local(fun(Values) -> [Value | Values] end),
           async_m:get_local()
       ]).
```

`local_ref/2` and `local/2` provide scoped access to another local context.
The lower-level `find_ref/1`, `get_ref/2`, `put_ref/2`, and `remove_ref/1`
functions operate directly on the callback/reference store.

## Waiting and callback forms

The simplest runner blocks until no registered callbacks remain:

```erlang
async_m:wait(Promise).
```

`wait_t/2` accepts:

```erlang
#{
    callback => Callback,
    cc => AsyncRContinuation,
    state => InitialState,
    offset => CallbackMapTupleIndex,
    timeout => MillisecondsOrInfinity
}
```

`callback` may have arity 0, 1, or 2. An arity-2 callback receives
`Callback(Reply, State)`. When it returns the same state shape, that value
becomes the next state; returning another shape completes with that value.

For custom tuple/record state, `offset` identifies the tuple element holding
the callback map:

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

## Integrating with an OTP process

`wait/1` is convenient at process boundaries but blocks the caller. To keep an
OTP server responsive, register the workflow with `exec/4` and route incoming
messages through `handle_info/3`.

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

## Transport adapters

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

`async_rpc:promise_call/4` calls a remote function. If the remote function
returns an `async_t`, the helper forwards its messages and final reply back to
the caller as a local promise.

```erlang
async_rpc:promise_call(Node, Module, Function, Args).
```

### Channels

`async_channel` provides a supervised request queue with a pool-size limit:

```erlang
{ok, _Pid} = async_channel:start(my_channel, 8),
Promise =
    async_gen_server:promise_channel_call(
      my_channel, Server, Request, Timeout).
```

### Raw monitored calls

```erlang
Ref = async:call(Process, Label, Request),
Promise = async:promise_mref(Ref, Timeout).
```

`Process` may be a PID, local registered name, `{global, Name}`, or
`{Name, Node}`.

## Architecture

The generic transformer stack is:

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

Responsibilities:

| Module | Responsibility |
| --- | --- |
| `async_m` | Primary identity-specialized API generated from `async_t` |
| `async_t` | Promise composition, reply handling, waiting, parallel aggregation |
| `async_r_t` | Runtime state, current local reference, callback-store access |
| `reply_t` | Message/final/error reply semantics |
| `async` | Monitored request transport and progress-message sending |
| `async_gen_server` | `gen_server` request adapter |
| `async_gen_fsm` | `gen_fsm` request adapter |
| `async_rpc` | Remote promise forwarding |
| `async_channel` | Supervised pooled request channel |
| `async_worker` | Supervised one-shot action worker |

Use `async_m` unless you specifically need to choose another inner monad. When
using `async_t` directly, pass the monad descriptor explicitly:

```erlang
AT = async_t:new(identity),
Promise = async_t:promise(RequestRef, AT),
ResultMonad = async_t:wait(Promise, AT),
Result = identity:run(ResultMonad).
```

## Advanced combinators and invariants

- `lift_reply/1` exposes messages and final replies as values to a higher reply
  layer.
- `lift_final_reply/1` exposes only final replies as values; messages continue
  outward.
- `with_message/2`, `with_final/2`, and `with_all/2` attach handlers at
  different reply boundaries.
- `handle_message/2` consumes messages with a callback/continuation.
- `pass/0` deliberately returns from the runtime layer without producing a
  final `async_t` reply.
- `par/1` is low level: only one branch may produce a final reply; other
  branches must emit messages or use `pass/0`.
- `progn_par/1` follows that low-level rule and returns the last branch's value.
- `map_promises/1,2` should be preferred for normal all-results aggregation.

Violating the single-final-reply invariant can invoke a continuation more than
once, leave internal counters behind, or complete an aggregate too early.

## Development

```shell
rebar3 compile
rebar3 ct
rebar3 xref
rebar3 dialyzer
```

The test suites cover transformer state, promise chaining, errors, messages,
timeouts, asynchronous accumulation, bounded concurrency, and historical API
versions.

For Docker-based compatibility testing across multiple Erlang/OTP versions:

```powershell
.\ci_scripts\sync_ci.ps1
.\ci_scripts\build.ps1
.\ci_scripts\run.ps1 -NoView
```

`ci_scripts/sync_ci.ps1` first runs `rebar3 get-deps`, then resolves the
selected `astranaut` dependency through `_build/default`. A fetched dependency
is read from `lib/astranaut`; a local checkout is discovered through the
`checkouts/astranaut/src` link, preserving uncommitted checkout changes. This
repository owns
`ci_scripts/ci-env.conf.example`; the ignored `ci_scripts/ci-env.conf`
contains local overrides. The sync script never replaces these
project-specific configuration files. Re-run it when the upstream CI file set
changes.

On Bash, use `bash ./ci_scripts/sync_ci.sh` instead.

On modern OTP releases, Dialyzer may report legacy opaque/generated-code
warnings that are independent of compilation and Common Test results.

## License

BSD 3-Clause License. See [LICENSE](LICENSE).
