# sf-migrate Design

## Context

sf-do/sf-cc 已经固定以下运行模型：

- pending stable node 是完整
  `#sf_callback{frame,acc_ref,parent_ref,timer_ref}`；
- frame 保存 `protocol`、`block`、`step_name`、`identity`、`module`、literal module
  `vsn`、generated `mfa` 和 capture-name map `env`；
- named identity 精确为 `{Protocol, Block, StepName}`，Vsn 不参与 identity；
- 每个 node 在真正执行前按自身 FrameOwner 查询 current manifest；
- successful Step 返回后，通过原 `parent_ref` 继续 linked continuation；
- ordinary callback 与 stable frame 是不同 encoding。

因此 sf-migrate 不需要也不应该在 StateHolder 的 `code_change/3` 中提前寻找、遍历或改写
frames。StateHolder 可能保存来自任意多个 FrameOwner 的 callback，本身也不知道这些 owner 将被
升级到哪个版本。reply-time owner resolution 才同时拥有 actual stored frame、current owner manifest
和实际 Outcome。

## Final Scope

sf-migrate 是 one-way source → current bridge。通常 source 是 historical/old module，current 是
已加载的新 module，因此本文称为 up migration。它处理：

- Block 改名；
- StepName 改名；
- Outcome pattern/shape 改变；
- target Step 所需 capture 不在 saved Env；
- capture 改名或同名 capture 的业务含义改变。

以下不属于本 change：

- whole-frame eager rewrite；
- StateHolder State 或 callbacks offset migration；
- reverse/down converter；
- target app/module Vsn discovery；
- release_handler private state、module-load registry或旧 BEAM执行；
- version graph、multi-hop composition或semantic-version ordering；
- module dependency declaration、循环检测或 release order enforcement；
- ordinary `do`/`#callback{}` migration。

## Ownership and Version

定义 `sf_do` 的 module 是 FrameOwner，也必须定义与该 frame 有关的 `sf_do_code_change/2` 和
`sf_migrate/3`。StateHolder module 即使保存 callback，也不拥有这些 declarations。

FrameOwner 只要定义 `sf_migrate`，就必须有显式：

```erlang
-vsn(v3).
```

`sf_migrate` 的第一个参数与 frame 中保存的值都使用该 literal module Vsn term。它可以是任意
literal Erlang term，不做 atom/binary/string coercion，也不替换为 app Vsn。没有显式 `-vsn`
时 parse transform 无法得到与 release tooling一致的稳定 source key，因此 migration-enabled
module 编译失败。

每个 supported source Vsn 都是一个 exact declaration。系统不猜测 predecessor，不从 current Vsn
推导路径，也不组合 `v1 -> v2 -> v3`。current v3 若兼容 v1 与 v2，就显式保留两份 historical
block declarations。

## Public Declaration

FrameOwner 提供：

```erlang
-export([sf_do_code_change/2, migrate_request2/2]).

sf_do_code_change(SourceVsn, SourceBlock) ->
    case {SourceVsn, SourceBlock} of
        {v1, order_flow} ->
            sf_migrate(
                v1,
                [async_m
                 || request1_v1,
                    Reply1 <- promise1(),
                    request2_v1,
                    Reply2 <- promise2_v1(Reply1),
                    finish_v1(Reply2)],
                #{
                    from => order_flow,
                    to => order_flow,
                    steps => #{
                        request1_v1 => #{to => request1},
                        request2_v1 => #{
                            to => request2,
                            convert => {?MODULE, migrate_request2, 2}
                        }
                    }
                });
        _ ->
            unhandled
    end.

migrate_request2(OldOutcome, OldEnv) ->
    {ok, NewOutcome, NewEnv}.
```

`sf_do_code_change/2` 是普通 Erlang router：可以使用多个 clauses、guards 或 `case`。runtime
传入 actual stored `SourceVsn` 与 `SourceBlock`。成功分支必须返回一个由 `sf_migrate/3` 产生的
opaque plan；不支持的 source 返回 `unhandled`。parse transform 在存在该 function 时自动加入 export。

`sf_migrate/3` 只允许出现在 `sf_do_code_change/2` 的返回路径中：

```erlang
sf_migrate(OldModuleVsn, CompleteOldComprehension, Spec)
```

- `OldModuleVsn`：exact literal source module Vsn；
- `CompleteOldComprehension`：原版本完整的 sf-do comprehension；
- `Spec.from`：该historical comprehension原本的named Block；
- `Spec.to`：当前 module 中现存的 named block；
- `Spec.steps`：old StepName 到 target descriptor 的 map；
- `target.to`：当前 StepName；
- `target.convert`：可选 `{Module, Function, 2}`。

Old comprehension 的 inline StepName grammar、placement、uniqueness 与 sf-do 完全相同。old/current block
都必须是 named block；unnamed frame 仍只支持 same-version普通恢复，不能进入 sf-migrate。

每个 `{OldModuleVsn, OldBlock}` 最多一个 declaration；每个 declaration 内 old StepName key 唯一。
target block/StepName 必须存在于 current sf-do analysis。用户不填写 generated function atom、source
index、AST hash、capture hash或anonymous block locator。

## What the Transform Derives

parse transform 复用 sf-do analyzer，从 old/current block 得到：

- `{Protocol, Block, StepName}`；
- old/current generated Step MFA；
- source/target Outcome pattern shape；
- source/target required capture-name sets；
- bridge 调用所需的 pattern binding 与 Env reconstruction metadata。

Outcome shape 是边界 pattern 的结构，不是远端 promise return type inference。transform 不分析被调用
module，不推断 term 的业务含义，也不生成 converter 业务代码。

没有 `convert` 时，mapping 只允许原样转发：

```text
OldOutcome shape compatible with NewOutcome shape
and TargetRequiredCaptures ⊆ SourceCaptureKeys
```

不满足时编译失败并要求显式 converter。capture value 的语义是否仍相同无法静态证明；保留同名
capture 或省略 converter 是用户作出的兼容承诺。

有 `convert` 时，用户函数负责同时转换 Outcome 与 Ctx：

```erlang
M:F(OldOutcome, OldEnv) ->
    {ok, NewOutcome, NewEnv}
  | {error, Reason}.
```

这里 Ctx 就是 generated Step `/2` 的第二参数，即 sf-do frame Env map；没有额外 HolderState、
app Vsn 或 migration context。若业务需要旧 State，它必须已经是 OldEnv 的显式 capture。

transform 只验证 MFA shape并生成 runtime target validators。它不会替用户生成 Outcome/Env 值转换。

## Generated Bridge

每个 explicit step mapping 生成一个 version-specific old-interface bridge。公开语义等价于：

```erlang
OldStepInterface(OldOutcome, OldEnv) ->
    case ConvertOrReuse(OldOutcome, OldEnv) of
        {ok, NewOutcome, NewEnv} ->
            CurrentStep(NewOutcome, NewEnv);
        {error, Reason} ->
            MigrationFailure(Reason)
    end.
```

实际 generated function 名称是内部实现，不进入 frame identity或用户 API。它必须包含 source Vsn，
或由同等的 plan id 区分，避免多个 historical Vsn 对同一 Block/Step 生成冲突。

bridge 不执行 old block，不重新发送 request，不重放已经完成的 qualifier，也不构造 FrameB。
它只在原 reply 已到达后，把 old Step 的调用边界适配到 current Step。

最重要的是：成功 bridge 不改写保存的 `#sf_callback` 或 frame。该 node 本来就是一次性消费；
bridge 直接调用 current generated Step，随后完全复用 sf-cc 的 parent_ref continuation flow。

## Reply-Time Resolution

每个 stable node 独立执行以下顺序：

```text
1. validate stored frame and load its FrameOwner current manifest
2. if StoredVsn =:= CurrentVsn:
       use normal sf-do resolution
3. otherwise ask Owner:sf_do_code_change(StoredVsn, StoredBlock)
4. if the returned exact plan explicitly maps StoredStep:
       validate source against CompleteOldComprehension and Spec.from
       invoke its generated bridge
5. otherwise try sf-do automatic named compatibility:
       same Protocol/Block/StepName
       and TargetRequiredCaptures ⊆ maps:keys(SavedEnv)
6. otherwise forward structured incompatible failure to direct parent_ref
```

explicit mapping 必须先于 cross-Vsn automatic inheritance，因为相同 identity 也可能显式声明
converter 来改变 Outcome/Env 业务语义。没有 exact plan或没有该 Step mapping时，automatic path
仍然可用；因此用户无需为没有变化的 block 编写 no-op declaration。

router 缺失、返回 `unhandled` 或没有 matching Step 都不是 exception，只表示没有 explicit plan。
损坏 plan、错误 owner/module、invalid converter return 属于 structured incompatibility；用户 converter
自身抛出的 exception 继续遵循 sf-cc 对用户 callback exception 的现有传播规则。

## Automatic Compatibility in Both Directions

Vsn mismatch 本身不表示不兼容。sf-do automatic compatibility 在 upgrade 与 downgrade 后都相同：

```text
same {Protocol, Block, StepName}
and current generated MFA exists
and CurrentRequiredCaptures ⊆ SavedEnv keys
```

例如 B → A downgrade：若 B 创建的 frame 保存的 Env 相对 A 只增加 capture、没有删除或改名 A
仍需要的 capture，则 A 可以忽略 extra Env fields并直接恢复。Outcome 不做 down conversion，原样传给
A Step；保持相同 named boundary 表示应用承诺该 Outcome 仍可被 A 接受。

如果 B 删除了 A 所需 capture、重命名 boundary，或改变 Outcome contract 却仍降到不认识 B 的 A，
该 callback 在触发时 failure。app downgrade 本身不因 pending callback 被阻止。

## Downgrade Boundary

down 顺序是 StateHolder 先 code-change，FrameOwner 后加载旧代码。StateHolder down 时目标
FrameOwner 仍未成为 current code，而且一个 Holder 可能含多个 owner。因此本 change 明确：

- app down 不调用 sf-migrate；
- 不调用 `fail_sf_callbacks/4`；
- 不按 saved/current Vsn 提前 fail callback；
- 不预生成 target candidates；
- 不转换 down Outcome 或 Env；
- 不读取 release_handler 私有状态。

FrameOwner 降级后，reply-time resolver 使用当时真正加载的 target manifest。兼容的 callback 继续，
不兼容的 callback 才 failure。这避免“block 未变化却因为 Vsn 不同被过度 fail”。

## Failure Contract

expected incompatibility 必须作为 callback Outcome 交给当前 node 的 direct `parent_ref`，不得抛异常、
定位 root/final Ref或修改其他 node：

```erlang
{error, {sf_runtime, {incompatible_frame, #{
    module => Module,
    stored_vsn => StoredVsn,
    current_vsn => CurrentVsn,
    identity => Identity,
    reason => no_compatible_plan
            | invalid_historical_frame
            | converter_failed
            | invalid_converter_result
            | incompatible_outcome
            | incompatible_env
}}}}
```

failure metadata 不得包含 Env、Outcome、State、callback value或其他敏感 payload。converter 返回的
business Reason MAY 作为 `converter_failed` 的受控 detail附带；文档必须提醒其会进入 failure Outcome。

sf-cc 负责 linked failure forwarding、一次性 node consumption、timer/monitor cleanup和显式 failure
handler。sf-migrate 不复制这些机制，也不提供 `on_mismatch`/replacement frame policy。

## Parse Transform Pipeline

在 sf-do 已有两阶段 transform 中增加最小步骤：

```text
1. analyze current sf_do blocks exactly as today
2. find sf_do_code_change/2 and sf_migrate/3 special forms
3. analyze each complete historical sf_do with the shared frontend
4. validate exact Vsn/block uniqueness and old/current Step mappings
5. derive Outcome/Env boundary shapes
6. generate current Step functions, version-specific bridges and opaque plans
7. preserve sf_do_code_change/2 ordinary Erlang routing, replacing each sf_migrate/3
   expression with its opaque generated plan value
8. emit the existing current '$async_sf_manifest'/0
```

不增加第二个 ordering-sensitive parse transform，不生成 historical workflow functions，也不在
current manifest 中保存完整 old AST。

## Release Ordering and Tooling

OTP appup/relup 由应用决定 module update order。本 change 不声明依赖、不禁止循环、不保证 callback
在 partial-upgrade窗口一定成功；在 owner 尚未到达兼容版本时触发 callback，代价只是该 callback 的
structured failure。

未来独立 Rebar plugin 可以读取 BEAM/manifest或做 whole-project static analysis，报告潜在 module
引用环和建议顺序。单个 parse transform 看不到 Promise 最终由哪个 module 定义，因而既不生成
`-sf_migration_dependencies`，也不承担全局分析。

## Compatibility Boundary

- 仅含 sf-do、没有 `sf_do_code_change/2` 的 module 行为不变；
- ordinary `do` 与 ordinary callbacks 行为不变；
- callback store、State layout和`#sf_callback{}` encoding不变；
- current manifest format/identity继续由 sf-do 定义；
- sf-migrate 不解决 async runtime 自身 encoding/protocol 的跨版本升级。
