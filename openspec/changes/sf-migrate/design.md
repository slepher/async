# sf-migrate Design

## Context

pending `#sf_callback{}` node 保存当前等待点之后的 frame 和 direct `next_ref`。升级时不能重新执行已经完成
的 step，也不能重新发起当前 pending request。迁移只需要把旧 frame 替换为指向当前
stable function 的新 frame：原 reply 到达后，从新的 continuation 继续。

`sf-do` 只使用 `{Protocol, BlockPath, StepName}` 命名 AsyncId 和 frame function。
identity 未变时 pending frame 自动继承；Action AST、source index、后续 continuation 和 captures 不参与身份
判断。`sf-migrate` 为 AsyncId 无法直接匹配的 boundary 提供显式 old/new 对应，也允许用户在
AsyncId/MFA 未变时显式声明 Env-only migration；后者只转换 capture-name map，不改变 identity。

downgrade 使用相反目标，但不反转术语：`old` 始终是 historical/较旧版本，`new` 始终是
当前编译/较新版本。upgrade 把 old frame 变为 new frame；downgrade 在较新代码仍运行时
把 new frame 变为 old frame，随后 OTP 才加载旧 callback module。

编译器只有当前源码，不具有旧 block 信息。因此历史 block 必须由用户显式提供。当前
block 已由 current `sf_do` 定义，可直接引用，不应再复制一份 current do block。
命名与匿名 block 使用统一 endpoint reference；匿名 endpoint 只暴露 `{Function, Arity, N}`。

## Goals

- 版本到 historical/current block 的映射显式、可审计。
- 历史 block 以完整 do block 输入，避免手写 generated IDs。
- step migration 是 allowlist，未列出即不迁移。
- automatic AsyncId match 默认不要求 declaration、version anchor 或 migration API；只有用户显式
  调用 migration API 且存在 directional same-identity Env plan 时，才在 resume 前转换 Env。
- runtime 用 descriptor 中的 block、step 和 capture ABI 验证历史数据。
- 成功迁移只替换每个 stable node 的 `#sf_callback.frame`，并保留 `next_ref` 及其余生命周期数据。
- mismatch 形成 failure Outcome并向 direct `next_ref` 转发；普通 success frame跳过，显式 failure handler可处理。
- migration 只在业务进程 `code_change/3` 显式调用时生效。
- downgrade 服从 OTP dynamic callback advanced update 的 suspend → current-code
  `code_change({down, TargetVsn}, ...)` → load old code → resume 时序。

## Non-Goals

- 不根据 AST 相似度自动建立 step mapping。
- 不为相同 AsyncId 建立无操作 migration plan；只有显式 Env migrator 的 same-identity mapping
  才生成 Env-only plan。
- 不从当前 block 反推历史 block。
- 不生成或调用 historical stable functions。
- 不加载、保留或执行旧 BEAM。
- 不迁移 legacy `#callback{}`。
- 不重放已完成 step、重新发送 request 或重建 monitor/timer。
- 不提供完整 frame runner 或 `async_sf_t`。

## Automatic Inheritance Boundary

对于 source frame，系统必须先判断 `sf-do` 的 exact named identity rule：

```text
same protocol
and same runtime BlockName
and same StepName
and target manifest contains the resulting AsyncId/MFA
    => automatically inherited
```

没有 same-identity Env-only plan 时，该路径不读取 migration manifest，不要求 historical block、
version anchor、step mapping 或 `migrate_sf_callbacks/4`。显式调用 migration API 且存在当前方向
Env-only plan 时，只在 resume 前转换 Env。async 之后的 continuation、bind pattern、CaptureAbi 和
Env runtime values 均不参与 identity 判断。

以下任一情况越过自动继承边界：

- explicit block 改名，或 anonymous lexical BlockPath 改变；
- StepName 改名；
- async boundary 被移到另一 BlockPath。

此时 runtime 必须默认失败，不得按最近 index、Action content 相似度、bind
name、CaptureAbi 或 Env value 自动选择 target。upgrade 不经迁移恢复时完成为
`{error, {appup, incompatible_frame}}`；downgrade 对应为
`{error, {appdown, incompatible_frame}}`。要继承该 request，必须由 `sf_migrate.steps`
显式将 old selector 映射到 new selector，并在 `code_change/3` 调用 migration API。

Capture/Env 兼容性与 async identity 正交。相同 AsyncId 在没有显式 Env-only plan 时自动继承；
若应用没有在 `code_change/3` 调用 migration API，目标 continuation 缺少 Env key 仍是 execution
environment failure，runtime 不得在 reply path 临时猜测 migration。用户可以为相同 AsyncId
显式 mapping 并按方向配置 `migrate_env`，用于变量改名、增加/删除 capture 或同名变量值语义转换；
该 plan 只替换 Env，不能把同一 AsyncId 重新定义为不同 async，也不能改变 MFA。

## Declaration Syntax

每个 historical block 使用完整 do block，StepName 以内联 atom marker 提供：

```erlang
sf_migrate(
    [
        async_m
     || request1_v1,
        Reply1 <- promise1(),
        async_m:modify_state(
            fun(S) -> S#state{status = request2_v1} end
        ),
        request2_v1,
        Reply2 <- promise2_v1(Reply1),
        promise3_v1(Reply2)
    ],
    #{
        transition => {{handle_call, 3, 0}, request_flow},
        block => block_1,
        versions => [v1, v2],
        current_versions => [v4],
        steps => [
            #{
                old => {step, request1_v1},
                new => {step, request1},
                directions => #{up => #{}, down => #{}}
            },
            #{
                old => {step, request2_v1},
                new => {step, request2},
                directions => #{
                    up => #{migrate_env => {?MODULE, migrate_reply2_env_up, 1}},
                    down => #{migrate_env => {?MODULE, migrate_reply2_env_down, 1}}
                }
            }
        ],
        on_mismatch => {?MODULE, on_sf_mismatch, 3}
    }
).
```

inline marker 的 atom literal、唯一性、placement 和 unnamed warning 规则与 `sf_do` 完全相同。
historical runtime BlockName 仍由 `transition` 的 historical endpoint 决定，不在 historical do
template 中重复填写。unnamed historical boundary 不进入 StepName selector table，不能成为 mapping
endpoint。

字段：

- `transition`：两个无方向 block endpoint；命名 endpoint 是 atom，匿名 endpoint 是
  `{Function, Arity, N}`。transform 根据 current manifest 确定当前端和 historical 端；
- `block`：historical block key，在该 logical block 内唯一；
- `versions`：锚定到该 historical block 的旧版本列表；
- `current_versions`：可选，显式锚定到 current block；同一个 name 只需声明一次；
- `steps`：old/historical selector 与 new/current selector 的配对；
- `directions`：step-level 方向 allowlist；存在 `up` 表示允许 old → new，存在 `down`
  表示允许 new → old。每个方向可提供独立 `migrate_env` external MFA；
- declaration/step-level `up_state`、`down_state`：对应方向 source Env 中的 State capture
  name；declaration-level 值作为所有 step 的默认值；
- declaration/step-level `state`：shorthand，等价于同时声明同名 `up_state` 和
  `down_state`；
- `on_mismatch`：可选 failure-policy external MFA。

第一版 historical do block 必须语法完整。用户“省略部分内容”的方式是省略不需要
迁移的 `steps` mapping；不得提交 partial do block，因为那会破坏 boundary、tail 和
capture ABI 分析。

同一 transition 可以有多个 declaration：

```text
block_1 versions [v1, v2]
block_2 versions [v3]
current current_versions [v4]
```

形成 anchors：

```erlang
#{v1 => block_1, v2 => block_1, v3 => block_2, v4 => current}.
```

version term 保持用户和 `code_change/3` 传入的精确 Erlang term，不进行 atom/binary/
string coercion。重复 version 指向不同 block 是编译错误；重复指向同一 block 可去重。

upgrade 与 downgrade 使用同一 declaration/runtime，但执行方向不同。对 dynamic callback
module，OTP upgrade 先加载较新 module，再由较新 module 执行 `code_change(Vsn, ...)`；
downgrade 则先由仍在运行的较新 module 执行
`code_change({down, TargetVsn}, ...)`，成功后才加载旧 module。该顺序与 Erlang/OTP
`appup` 的 advanced `update` 指令一致：

- https://www.erlang.org/docs/29/apps/sasl/appup
- https://www.erlang.org/doc/system/appup_cookbook.html

因此 v4 declaration 同时编译 v3 historical metadata 和 v4 current metadata。upgrade
使用 v3 metadata 验证 source、用 v4 current manifest 构造 target；downgrade 使用 v4
current manifest 验证 source、用 v3 historical metadata 构造尚不执行的 target frame。
匿名 endpoint `{F, A, N}` 统一展开为 `'-F/A-fun-N-'`。两个方向必须显式配置，且不能
假设 reverse Env migration 可由 forward migrator 自动求逆。

## Step Selectors

selector 只支持 source 显式声明的 StepName：

```erlang
{step, StepName}
```

`{step, StepName}` 直接解析 historical/current BlockPlan 中的唯一 StepName，不依赖 Action AST、
source index、bind variable name 或 qualifier kind，也不接受 Offset。零匹配报
`sf_step_not_found`，重复名称在 block analysis 阶段报 duplicate StepName。

old 与 new selector 分别在 historical block 和 current block 编译，其版本角色不会随
runtime direction 反转。解析结果固定为 `{HistoricalBlockKey, OldAsyncId}` 与 current
`NewAsyncId`：up plan 以 OldAsyncId 为 source、NewAsyncId 为 target；down plan 以 NewAsyncId
为 source、OldAsyncId 为 target。runtime 不保存或重新解析 selector。用户不得填写 generated
function atom、AsyncId 或 CaptureAbi。

若 resolved old/new AsyncId 与 runtime BlockName 都相同，并且所有 enabled direction 都没有
`migrate_env`，该 mapping 是冗余 automatic inheritance；compiler MUST 报
`redundant_sf_migration_mapping`。只要至少一个 enabled direction 显式配置 `migrate_env`，该
mapping 就是 Env-only migration plan：target identity/MFA 保持不变，只按该方向生成 TargetEnv。
same-identity mapping 只为实际配置 `migrate_env` 的 direction 生成 plan；同 mapping 中没有 migrator
的另一方向继续 automatic inheritance，不生成 no-op plan。
显式 mapping 的其他用途是连接 old/new AsyncId 不同的 boundary。

StepName 重命名使用 old/new `{step,...}` 显式连接：

```erlang
#{
    old => {step, load_user},
    new => {step, fetch_user},
    directions => #{up => #{}, down => #{}}
}
```

后续版本保持相同 BlockPath/StepName 时走 automatic inheritance。

mapping schema 固定为 map：

```erlang
#{
    old => step_selector(),
    new => step_selector(),
    state => undefined | atom(),       %% optional shorthand
    up_state => undefined | atom(),    %% optional
    down_state => undefined | atom(),  %% optional
    directions => #{
        up => #{migrate_env => undefined | {module(), atom(), 1}},
        down => #{migrate_env => undefined | {module(), atom(), 1}}
    }
}.
```

不同时支持 tuple shorthand，避免文档和实现出现多套等价格式。

`directions` 中 key 的存在性就是 allowlist；至少必须存在 `up` 或 `down`。同 ABI 时方向
config 可以是空 map。plain step-level `migrate_env` 不再接受，因为它无法表达反向函数。

State 配置先在 declaration 和 step 各自展开 shorthand：

```text
state = S, up_state absent   -> up_state = S
state = S, down_state absent -> down_state = S
state = S, directional = S   -> valid
state = S, directional != S  -> sf_state_capture_conflict
```

再分别计算每个 step 的 effective direction state：

```text
declaration direction absent + step direction absent -> undefined
declaration direction absent + step direction S      -> S
declaration direction G      + step direction absent -> G
declaration direction G      + step direction G      -> G
declaration direction G      + step direction S != G -> sf_state_capture_conflict
```

declaration default 只在单个 `sf_migrate` declaration 内生效，不按 `versions` 分支，也不跨
其他 declarations 传播。`up_state` 在 up source（historical/old）capture layout 中校验；
`down_state` 在 down source（current/new）capture layout 中校验。manifest 只保存归一化后
的 effective value，不保存 shorthand 或继承关系。

## Compiler Pipeline

`sf-migrate` 扩展 `async_sf_transform`，不新增第二个有 ordering 依赖的 parse transform：

```text
1. scan sf_do and build current BlockPlans
2. scan sf_migrate declarations
3. analyze each complete historical block and inline markers with the same boundary frontend
4. resolve old/current `{step,StepName}` selectors
5. normalize declaration/step state defaults and directional allowlists
6. record capture compatibility and optional per-direction migration MFAs
7. generate current stable functions
8. generate current manifest and bidirectional migration manifest
9. remove sf_migrate declaration expressions from executable code
```

historical analysis只生成 metadata，不生成 historical functions、exports 或 program。

runtime implementation 保持现有增量边界：`async_m:migrate_sf_callbacks/4` 只做 facade，
编排放在独立 `async_sf_migration`；复用 `async_t:state_callbacks_gs`、
`async_util:same_type_state`、`async_sf_runtime:stable_term/1`、
`async_sf_runtime:complete/2` 和 Erlang 原生 map exact lookup。不得把 migration 编排并入
`async_sf_runtime`，也不得复制 State merge、stable-term 或 completion helper。

## Migration Manifest

仅当 module 至少声明一个 `sf_migrate` 时才导出固定入口。用户已定义同名同 arity function
时，transform 必须报告 compile-time collision，不得覆盖或生成重复 definition：

```erlang
'$async_sf_migration_manifest'() ->
    #{
        format => 1,
        protocol => 1,
        module => ?MODULE,
        sources => #{
            '-handle_call/3-fun-0-' => request_flow
        },
        blocks => #{
            request_flow => #{
                anchors => #{
                    v1 => block_1,
                    v2 => block_1,
                    v3 => block_2,
                    v4 => current
                },
                historical => #{
                    block_1 => #{
                        endpoint => '-handle_call/3-fun-0-',
                        asyncs => #{
                            OldAsyncId1 => #{
                                step_name => request1_v1,
                                mfa => {?MODULE, OldAsyncFunction1, 2},
                                capture_abi => OldAbi1
                            }
                        }
                    },
                    block_2 => HistoricalMetadata2
                },
                plans => #{
                    up => #{
                        {block_1, OldAsyncId1} => #{
                            target_async => NewAsyncId1,
                            source_capture_abi => OldAbi1,
                            target_capture_abi => NewAbi1,
                            source_state_capture => undefined,
                            migrate_env => undefined
                        }
                    },
                    down => #{
                        {block_1, NewAsyncId1} => #{
                            target_block => '-handle_call/3-fun-0-',
                            target_async => OldAsyncId1,
                            target_mfa => {?MODULE, OldAsyncFunction1, 2},
                            source_capture_abi => NewAbi1,
                            target_capture_abi => OldAbi1,
                            source_state_capture => undefined,
                            migrate_env => undefined
                        }
                    }
                },
                on_mismatch => {?MODULE, on_sf_mismatch, 3}
            }
        }
    }.
```

up target MFA 不复制到 plan；runtime 用 `target_async` 查询当前 `$async_sf_manifest/0`，
据此构造 new/current frame。down target 尚未成为 loaded current code，不能通过 module
call 查询其 manifest，因此 historical analyzer 必须保存由完整旧 block 确定的 runtime
BlockName、AsyncId、MFA 和 CaptureAbi。down 只把这些数据写入 frame，不调用 historical
MFA；旧 module 加载后，普通 frame validation 会再次对照旧版 current manifest。

本功能尚未发布，因此上面的 directional schema 是唯一受支持的 format 1。旧开发草案的
flat `plans` 结构不属于兼容输入，不增加 format/protocol 版本，也不实现 dual reader；
compiler 与 runtime 必须在同一变更中原子切换到该 schema。

## Runtime API

业务进程在 OTP advanced update 的 `code_change/3` 中显式调用。upgrade 时较新代码已加载；
downgrade 时仍由较新代码执行，成功返回后 OTP 才加载旧代码：

```erlang
code_change({down, TargetVsn} = Direction, StateNew, Extra) ->
    {ok, StateOld, OldCallbacksOffset} =
        downgrade_business_state(TargetVsn, StateNew, Extra),
    migrate_code_change(Direction, OldCallbacksOffset, StateOld);
code_change(SourceVsn, StateOld, Extra) ->
    {ok, StateNew, NewCallbacksOffset} =
        upgrade_business_state(SourceVsn, StateOld, Extra),
    migrate_code_change(SourceVsn, NewCallbacksOffset, StateNew).

migrate_code_change(Direction, TargetOffset, TargetState) ->
    case async_m:migrate_sf_callbacks(
        ?MODULE,
        Direction,
        TargetOffset,
        TargetState
    ) of
        {ok, State1, _Results} ->
            {ok, State1};
        {unhandled, State1, _Results} ->
            {ok, State1};
        {error, Reason, _UnchangedState, _Results} ->
            {error, {sf_migration_failed, Reason}}
    end.
```

示例中的 business-state helpers 由应用实现。它们必须保留 callbacks map，并返回 target
State layout 对应的 callbacks offset。upgrade 使用新版 offset；downgrade 必须使用旧版
target offset，不能把较新 module 编译出的 `#state.callbacks` ordinal 套到旧 State tuple。

API contract：

```erlang
async_m:migrate_sf_callbacks(Module, DirectionVsn, TargetOffset, TargetState) ->
    {ok, NewState, [migration_result()]}
  | {unhandled, State, [migration_result()]}
  | {error, error_info(), TargetState, []}.
```

`DirectionVsn = SourceVsn | {down, TargetVsn}`，与 OTP `code_change/3` 第一参数一致：普通
Vsn 表示 upgrade source version，`{down, Vsn}` 中 Vsn 表示 downgrade target old version。
第四参数是应用已转换成 target layout 的 State，并且是所有 Env migrator 看到的
`current_state` 原值。

upgrade 没有 stable entries、没有相关 declaration 或 source version 未锚定时可返回
`unhandled`。downgrade 没有 stable entries也可返回 `unhandled`；但只要存在 pending
current/new frame，target version 未锚定、没有 down plan 或无法构造 historical target
都必须阻止 code change，不能把 new frame 原样留给旧 module。

每个成功、abort 或 unhandled entry 使用固定 result shape：

```erlang
#{
    ref => Ref,
    direction => up | down,
    version => SourceVsn | TargetVsn,
    source_block => atom(),
    source_async => binary(),
    target_block => atom() | undefined,
    target_async => binary() | undefined,
    status => migrated | already_current | aborted | unhandled,
    reason => mapped
            | current_anchor
            | no_declaration
            | unanchored_upgrade_source
            | #{
                  mismatch => mismatch_reason(),
                  policy => explicit | forced,
                  dispatch => forwarded | handled | unhandled | {failed, term()}
              }
}.
```

`source_*` 永远描述 API 入口 callback 中的 frame；`target_*` 永远描述成功后将写入的
frame。current-anchor refresh 的 source/target block 和 step 相同。无法解析 target 的
unhandled/abort result 使用 `undefined`，不得让同一个 `block` field 随执行分支改变含义。

API-level error 固定为：

```erlang
#{
    stage => manifest_preflight
           | target_state_preflight
           | frame_preflight
           | migration_processing
           | state_commit,
    reason => {missing_manifest, current | migration}
            | {invalid_manifest, current | migration, term()}
            | {module_mismatch, current | migration, module(), term()}
            | {invalid_target_offset, term()}
            | invalid_callbacks_map
            | {unanchored_downgrade_target, term()}
            | {invalid_source_frame, term()}
            | down_async_not_mapped
            | {invalid_historical_target, term()}
            | {unhandled_downgrade_mismatch, mismatch_reason()}
            | {state_commit_failed, term()},
    direction => up | down,
    version => SourceVsn | TargetVsn,
    ref => term() | undefined,
    source_block => atom() | undefined,
    source_async => binary() | undefined,
    target_block => atom() | undefined,
    target_async => binary() | undefined
}.
```

stage/reason 组合必须匹配：manifest errors 使用 `manifest_preflight`，offset/callbacks
layout 使用 `target_state_preflight`，anchor/source/plan/target metadata 使用
`frame_preflight`，未被 policy 处理的 runtime mismatch 使用 `migration_processing`，setter
failure 使用 `state_commit`。error tuple 中的 State 必须与 API 第四参数逐项相同，Results
必须是 `[]`；因为没有结果被提交，不能返回看似已生效的 per-entry results。相同输入按
callbacks key 的 Erlang term order 选择第一个 error，保证诊断确定。reason detail 可以
携带 manifest key、offset、MFA 或 ABI 等定位数据，但不得包含完整 State、Env 或 callback map。

## Per-Entry Migration

runtime 遍历共享 callbacks map 中所有 stable `#sf_callback{}` nodes，包括 ActionMRef 下的 current
node与 internal ContinuationRef 下的 outer nodes。legacy `#callback{}` 和 local accumulator values保持不变。

upgrade：

```text
historical/old source frame
  -> SourceVsn selects historical anchor
  -> first accept exact current BlockName/AsyncId as sf-do automatic inheritance
  -> otherwise validate source AsyncId/ABI against historical metadata
  -> find up plan by {HistoricalBlock, OldAsyncId}
  -> migrate/reuse Env
  -> resolve NewAsyncId/MFA/ABI from current manifest
  -> build new/current frame
```

downgrade：

```text
current/new source frame
  -> {down, TargetVsn} selects historical target anchor
  -> validate source AsyncId/ABI against current manifest
  -> accept exact historical BlockName/AsyncId as automatic inheritance
  -> otherwise find down plan by {HistoricalBlock, NewAsyncId}
  -> migrate/reuse Env
  -> resolve OldAsyncId/MFA/ABI from historical metadata
  -> build old/historical frame without executing its MFA
```

两种方向都只替换 `sf_callback.frame`。runtime MUST NOT 递归进入某个 callback value寻找 outer frame；
outer frame若存在，必定作为独立 node由同一次 deterministic enumeration处理。

成功时保留：

- callbacks map key/request correlation key；
- direct `next_ref`；
- `acc_ref`；
- `timer_ref` 及剩余 timer；
- monitor；
-已经发出的 request。

不调用 source frame，不执行 target frame，不重新计时。downgrade target MFA 只有在 OTP
加载旧 module 并恢复 process 后，才可能由原 reply 触发。

## Capture Environment Migration

frame Env 是 capture-name atom 到运行时值的 map：

```erlang
#{'Reply1' => Reply1, 'UserId' => UserId}.
```

CaptureAbi 直接等于 `lists:sort(maps:keys(EnvLayout))` 对应的 capture-name atom list，不做
SHA/digest。变量改名会改变 ABI；同名变量含义变化可能保持 ABI，但用户仍可显式配置 Env-only
migrator 转换 value。

每个 enabled direction 独立判断 source/target CaptureAbi。ABI 相同且该方向未配置
`migrate_env` 时原样复用 Env；ABI 不同则该方向必须配置自己的 migration MFA。up/down
MFA 都固定接收 context map，不能把 up MFA 反向调用：

```erlang
Module:Function(MigrationContext) ->
    {ok, TargetEnv}
  | {error, Reason}.
```

普通 mapping 的 context 只有：

```erlang
#{env => SourceEnv}.
```

State capture 可在 declaration 或 step mapping 声明：

```erlang
#{
    state => 'State',
    %% 等价于：
    up_state => 'State',
    down_state => 'State'
}.
```

不能同时给出互相冲突的 shorthand/directional 值。declaration-level directional value 是
所有 step 的 default；step 可省略或重复同方向值，不能改成不同 capture name。up 的
effective capture 在 historical/old source layout 中验证；down 的 effective capture 在
current/new source layout 中验证。

启用方向配置 migrator 且该方向存在 effective State capture 时，context 精确为：

```erlang
#{
    env => SourceEnv,
    source_state => maps:get(EffectiveDirectionState, SourceEnv),
    current_state => StatePassedToMigrateSfCallbacks
}.
```

`source_state` 始终表示迁移源 frame 捕获的 State：upgrade 时来自 old/historical Env，
downgrade 时来自 new/current Env；它不随 old/new 的版本角色改变含义。

`current_state` 精确等于调用
`async_m:migrate_sf_callbacks(Module, DirectionVsn, TargetOffset, TargetState)` 时传入的第四
参数原值。runtime 不得先清空 callbacks field、再次执行 State appup/appdown、应用
mismatch callback 更新，或用 working State 替换该值。同一次 API 调用的所有 Env
migrator 必须看到同一个 snapshot。应用应先把业务 State 转成 target layout：upgrade
传入 new State，downgrade 传入 old target State。

State 指令只负责识别并提供 context fields。runtime 不得自动把 `current_state` 写入
target Env，也不得自动调用业务 State converter。未声明当前方向 state 时，context 中
不得出现 `source_state` 或 `current_state`；即使 SourceEnv 含名为 `'State'` 的 key，
runtime 也必须把它当作普通 capture。

MFA 必须是当前 module version 可 external-call 的函数。返回 TargetEnv 后 runtime 必须：

- 验证它是 capture-name atom map；
- 验证 target frame metadata 来自该方向的权威 metadata；
- 使用 target CaptureAbi 构造新 frame。

Env 原则上不应包含 anonymous/local fun。State/Env 中的旧 fun 在 code change 时不会被自动清除或
替换；即使源码文本未变化，它仍引用创建它的 code version，old code purge 后可能不可调用。
若应用必须保留 fun-valued capture，正确责任边界是 explicit Env migrator 在 target code 中重新
生成 fun，或改写为 external MFA/data descriptor，并在以后每次可能 purge defining code 的迁移中
重复该过程。

当前 sf-migrate initiative 不强制实现 local-fun regeneration support。base implementation MAY
继续把包含 local fun 的 TargetEnv 归为 `unstable_capture`；未来实现若选择支持，只能接受显式
Env migrator 返回的 target-version fun，不得把 SourceEnv 中的旧 fun自动视为可继承，也不得把
fun body source-text equality 当作兼容依据。

ABI 不同但没有 MFA、MFA 抛异常、返回格式错误或 TargetEnv 不稳定，均进入 runtime
mismatch。compiler 即使从声明的 historical block 看见 ABI 不同，也必须允许缺少 MFA 的
mapping 编译并在 plan 中保存 `migrate_env => undefined`；真正运行中的 source frame/Env
只可由 runtime 验证，且显式 `on_mismatch` 必须有机会处理。

## Mismatch Policy

可配置 failure-policy callback：

```erlang
on_sf_mismatch(Info, State, NextRef) ->
    {abort, NewState}.
```

`Info` 至少包含：

```erlang
#{
    module => Module,
    direction => up | down,
    version => SourceVsn | TargetVsn,
    anchor => HistoricalBlock | undefined,
    source_block => SourceBlock,
    source_async => SourceAsyncId,
    source_capture_abi => SourceAbi,
    target_block => TargetBlock | undefined,
    target_async => TargetAsyncId | undefined,
    callback_ref => Ref,
    reason => mismatch_reason()
}.
```

```erlang
mismatch_reason() ::
    historical_async_not_found
  | async_not_mapped
  | capture_abi_mismatch
  | env_migration_failed
  | unstable_capture
  | current_async_not_found
  | current_capture_abi_mismatch
  | unanchored_downgrade_target
  | invalid_historical_target.
```

callback 是 migration failure policy，不是 ordinary async callback：

- `{abort, NewState}`：在 working map 中消费 current node及其direct resources，并把精确 Outcome
  `{error, {async_aborted, {sf_migration, Info}}}` dispatch 给 original direct `next_ref`；
- callback 抛异常或返回其他值时转为 forced abort。

mismatch callback 不允许返回 replacement frame。所有可继续执行的新 frame 都必须来自
编译期 explicit step mapping 和 Env migration，不能由 failure policy 绕过 allowlist。

upgrade 未配置 `on_mismatch` 时产生 forced failure并向 direct `next_ref` 转发。downgrade 未配置 callback 时，任何
无法映射的 pending current frame必须返回 error 并阻止加载旧 module；只有显式
`on_mismatch` 返回 abort，才允许消费该 pending node后继续 downgrade。mismatch MUST 跳过 ordinary
success frame，但 MAY 被 linked chain中的显式 failure handler消费；不得直接定位 legacy/root final callback。

upgrade source version 未锚定时 entry 可保持不变并返回 unhandled。downgrade target
version 未锚定时不能保留 current/new frame，必须在任何修改前返回 error。

## State Update and Failure Semantics

migration 在 `code_change/3` 调用期间同步执行：

1. 保存 API 第四参数原值作为所有 migrator 的 `current_state`；
2. 读取 callbacks map snapshot；
3. preflight manifest、target offset 和所有 direction plans；
4. 对每个 stable entry 构造 working map 和 per-entry result；
5. mismatch callback 返回的业务 State 按 callbacks key 的 Erlang term order 串行传递，但
   callbacks field 由
   migration runtime 管理；
6. 最后通过 target offset setter 一次写回 working callbacks map；
7. 对 abort entry 从其 direct `next_ref` 执行 failure dispatch。

单个 entry mismatch 不导致已成功迁移的其他 entry 回滚。基础设施错误（manifest
格式错误、offset 无效等）在修改前返回 `{error, ...}`；这类错误不得部分写回 State。
downgrade 中的未锚定 target、missing down plan 或无效 historical target 也属于 preflight
failure；不得先取消 timer、dispatch failure 或提交部分 frame 后再拒绝 downgrade。

Env migrator 或 mismatch policy 的 runtime failure 也必须先在 working data 上完成；若其
导致 API-level error，runtime 丢弃全部 working State/results，返回入口 TargetState 和空
Results，并且不得取消 timer/monitor、dispatch failure 或提交 frame。migrator/policy 是
用户代码，runtime 不承诺回滚其自行产生的外部副作用；使用方应保持这些 callback
side-effect-free。

## User Documentation

README 必须按最终公开 contract 提供 declaration/options、named/anonymous transition、
selector、up/down Env migrator、State context、current refresh、OTP appup/downgrade 调用顺序、
result/error shape 和故障排查示例。只描述公开行为，不暴露私有 helper 或 transform 中间结构。
每个纵向功能 task 同步更新自己引入的 README surface，避免最后用文档掩盖 contract 漂移。

## Compatibility Boundary

- 未调用 migration API 时不读取 migration manifest，不扫描 callbacks。
- legacy entries 永不由本 API 修改。
- upgrade 中没有相关 declaration 或未锚定 source version 的 stable entry保持原样；
  downgrade 不允许把 current/new stable frame原样交给旧 module。
- manifest 只保存 metadata/MFA，不保存 historical executable closure。
- `sf-do` 的 current functions、普通 async API、callbacks field 和 `async_r_t` 不因
  migration 改变语义。
