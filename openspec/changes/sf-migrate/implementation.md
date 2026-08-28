# sf-migrate Implementation Contract

本变更必须在 `sf-do` 实现和测试通过后开始。实现直接消费 `sf-do` 定义的 frame、
`#sf_callback{}`、current manifest 和 `async_sf_transform`，不得复制这些协议。

## 0. Responsibility Boundary

`sf-do` 已负责 named frame 的 exact automatic inheritance：有 inline StepName 的 frame 使用
protocol、Block 和 StepName；unnamed frame 在 migration 中直接 incompatible。
Protocol/Block/StepName/MFA未变时 pending frame 默认不经本 API 直接进入目标 continuation。实现 MUST NOT
为该路径生成或要求无操作 migration plan；但显式 directional `migrate_env` MUST 可生成
same-identity Env-only plan。

`sf-migrate` 处理上述 identity 变化导致 source/target Block/Step不同的 boundary mapping，也处理
显式配置 `migrate_env` 的 same-identity Env-only mapping。不同identity没有mapping时 MUST
默认失败；compiler/runtime MUST NOT 用 index 接近、AST/capture 相似度、bind name 或 runtime
value 猜测 target。CaptureAbi 和 Env 只用于构造目标执行环境，不得改写 automatic identity rule；
same-identity plan 必须保持Protocol/Block/StepName/MFA。

## 1. Input Contract

```erlang
sf_migrate(
    [async_m || HistoricalQualifiers],
    #{
        transition => {BlockRefA, BlockRefB},
        block => HistoricalBlockKey,
        versions => [VersionTerm],
        current_versions => [VersionTerm],          %% optional
        state => StateCaptureName,                  %% optional shorthand
        up_state => StateCaptureName,               %% optional
        down_state => StateCaptureName,             %% optional
        steps => [StepMapping],
        on_mismatch => {Module, Function, 3}         %% optional
    }
).
```

```erlang
-type block_ref() :: atom() | {Function :: atom(), Arity :: non_neg_integer(), N :: non_neg_integer()}.
```

`StepMapping` 只允许 map：

```erlang
#{
    old => step_selector(),
    new => step_selector(),
    state => undefined | atom(),       %% optional shorthand
    up_state => undefined | atom(),    %% optional
    down_state => undefined | atom(),  %% optional
    directions => #{
        up => direction_config(),
        down => direction_config()
    }
}.

-type direction_config() :: #{migrate_env => undefined | {module(), atom(), 1}}.

-type step_selector() ::
    {step, atom()}.
```

要求：

- historical block 必须是完整、非空、Monad 为 `async_m` 的 do block，只接受 `sf_migrate/2`；
- historical StepName 使用与 `sf_do` 相同的 inline literal atom marker grammar；marker 不执行，
  duplicate/invalid placement 报 error，unnamed boundary warning 且不进入 selector table；historical
  runtime BlockName 从 transition endpoint 取得；
- `transition` 两端中必须恰有一个解析到 current manifest；
- 匿名 `{F, A, N}` 必须解析为对应non-migratable local locator，不得接受generated function atom或hash；
- `block` 在该 name 内唯一且不能使用保留 atom `current`；
- `versions` 必须非空；
- `current_versions` 可省略，并且同一 name 的 declarations 合并；
- version term 使用 exact equality；
- old/new selector 的版本角色固定：old 属于 historical block，new 属于 current block；
- `directions` 至少包含 up/down 之一；up 表示 old → new，down 表示 new → old；
- 每个方向独立提供 `migrate_env`；不得接受方向不明的 plain step-level `migrate_env`；
- selector 只接受 `{step,StepName}`，并必须在对应 historical/current block 中存在；
- mapping old selector 不得重复；
- resolved old/new Protocol/Block/StepName/MFA都相同、且所有 enabled direction 均无
  `migrate_env` 时报 `redundant_sf_migration_mapping`；至少一个方向有 migrator 时生成 Env-only plan；
  same-identity mapping 只为含 migrator 的 direction 生成 plan，其他 direction 不生成 no-op plan；
- declaration 和 step 的 `state`、`up_state`、`down_state` 若存在必须是 literal atom；
  `state => S` 展开为两个 directional values，显式 directional value 与 S 不同时报
  `sf_state_capture_conflict`；
- declaration directional state 是所有 step 的 default；step 只能继承或重复同方向值，
  不得覆盖为不同值。default 不按 versions 分支，也不跨 declarations 传播；
- effective up_state 必须属于 resolved old step capture layout；effective down_state 必须
  属于 resolved new step capture layout；
- `migrate_env` 和 `on_mismatch` 必须是 literal external MFA。

CaptureAbi 不同且 enabled direction 未配置 `migrate_env` 不是 compile error。compiler 必须
保留 `migrate_env => undefined`；真正运行中的旧 frame/Env 不可由新版 compiler 访问，
runtime 才把该情况归为 `capture_abi_mismatch` 并进入 directional mismatch policy。

declaration 只用于编译 metadata，transform 后不得残留运行时 `sf_migrate/2` 调用。

## 2. Transform Extension

在 `async_sf_transform` 中增加阶段：

```text
collect current sf_do
  -> build current BlockPlans
  -> collect sf_migrate
  -> analyze historical complete blocks with shared analyzer
  -> merge/check version anchors
  -> resolve selectors
  -> normalize shorthand/default directional state
  -> cross-check direction allowlists, capture ABI and MFA declarations
  -> generate both manifests and current stable functions
```

historical analysis 必须复用 `sf-do` 的 qualifier boundary、inline marker parser、named Block/Step identity和
CaptureLayout。CaptureAbi 直接使用排序后的 capture-name atom list。historical plan 不生成
function/export，也不建立 Action canonicalizer、source index 或 content digest。

`async_m:migrate_sf_callbacks/4` 必须保持薄 facade。manifest preflight、entry migration、
mismatch 和 commit orchestration 保持在独立 `async_sf_migration`，并复用现有
`async_t:state_callbacks_gs`、`async_util:same_type_state`、
`async_sf_runtime:stable_term/1` 与 `async_sf_runtime:complete/2`。不得复制这些能力或把
migration 大段并入 `async_sf_runtime`。

## 3. Selector Resolution

shared analyzer 必须为 historical/current block 的 named boundaries 建立唯一StepName → boundary metadata table。
`{step, StepName}` 直接选择对应BlockPlan中的boundary；它不依赖 Action AST、source index、
bind variable name 或 qualifier kind，也不接受 Offset。缺失时报 `sf_step_not_found`，重复名称在
block StepNames validation 阶段报错。

解析结果必须在编译期固定为 `{HistoricalBlockKey, OldStep}` 和 current
`{CurrentBlock, NewStep}`。up plan 以historical Block/OldStep为source，down plan以current
Block/NewStep为source；runtime 不解析源级
selector，也不因 direction 反转 old/new 的版本角色。runtime 不按 AST/index/name 猜测 target。

错误：

- `sf_current_block_not_found`；
- `sf_transition_endpoint_mismatch`；
- `duplicate_sf_historical_block`；
- `sf_version_anchor_conflict`；
- `sf_step_not_found`；
- `invalid_sf_step_names`；
- `unknown_sf_step_record`；
- `sf_step_name_count_mismatch`；
- `sf_state_capture_conflict`；
- `invalid_sf_state_capture`；
- `invalid_sf_migration_direction`；
- `duplicate_sf_step_mapping`；
- `invalid_sf_migration_mfa`；
- `invalid_sf_migrate_syntax`。

## 4. Generated Manifest

固定导出：

```erlang
-export(['$async_sf_migration_manifest'/0]).
```

该 export/function 只在存在 migration declaration 时生成。用户 module 已定义
`'$async_sf_migration_manifest'/0` 时必须报告明确 collision diagnostic。

manifest 顶层必须包含：

```erlang
#{
    format => 1,
    protocol => 1,
    module => Module,
    sources => #{SourceBlockName => CurrentBlockName},
    blocks => #{BlockName => BlockMigration}
}.
```

每个 `BlockMigration`：

```erlang
#{
    anchors => #{VersionTerm => current | HistoricalBlockKey},
    historical => #{
        HistoricalBlockKey => #{
            endpoint => HistoricalRuntimeBlockName,
            steps => #{HistoricalStepName => #{
                mfa => {Module, HistoricalStepFunction, 2},
                capture_abi => OldAbi
            }}
        }
    },
    plans => #{
        up => #{
            {HistoricalBlockKey, OldStep} => #{
                target_step => NewStep,
                source_capture_abi => OldAbi,
                target_capture_abi => NewAbi,
                source_state_capture => undefined | UpStateCaptureName,
                migrate_env => undefined | {M, F, 1}
            }
        },
        down => #{
            {HistoricalBlockKey, NewStep} => #{
                target_block => HistoricalRuntimeBlockName,
                target_step => OldStep,
                target_mfa => {Module, HistoricalStepFunction, 2},
                source_capture_abi => NewAbi,
                target_capture_abi => OldAbi,
                source_state_capture => undefined | DownStateCaptureName,
                migrate_env => undefined | {M, F, 1}
            }
        }
    },
    on_mismatch => undefined | {M, F, 3}
}.
```

up target MFA/ABI 在 runtime 从 `'$async_sf_manifest'/0` 按CurrentBlock/NewStep获取。down 在 old module
加载前执行，target block/MFA/ABI 必须来自 historical metadata；runtime 只构造 frame，
不得调用 historical target MFA。

功能尚未发布；本节 directional schema 直接定义唯一的 `format => 1, protocol => 1`。
旧开发草案 flat plan 不受支持，不增加版本、不生成兼容 reader，transform/runtime 同时切换。

## 5. Runtime API

```erlang
-spec async_m:migrate_sf_callbacks(
    module(), DirectionVsn :: term(), TargetOffset :: integer(), TargetState
) ->
    {ok, State, [migration_result()]}
  | {unhandled, State, [migration_result()]}
  | {error, error_info(), TargetState, []}.
```

entry processing result 至少包含：

```erlang
#{
    ref => Ref,
    direction => up | down,
    version => SourceVsn | TargetVsn,
    source_block => SourceBlock,
    source_step => SourceStep,
    target_block => TargetBlock | undefined,
    target_step => TargetStep | undefined,
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

source fields 总是入口 frame；target fields 总是 committed/intended frame，无法解析时为
`undefined`。current refresh 两组 fields 相同。不得保留语义随路径变化的 `block` field。

`error_info()` 固定为：

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
            | down_step_not_mapped
            | {invalid_historical_target, term()}
            | {unhandled_downgrade_mismatch, mismatch_reason()}
            | {state_commit_failed, term()},
    direction => up | down,
    version => SourceVsn | TargetVsn,
    ref => term() | undefined,
    source_block => atom() | undefined,
    source_step => atom() | undefined,
    target_block => atom() | undefined,
    target_step => atom() | undefined
}.
```

manifest/target-state/frame/processing/commit failure 必须分别使用
`manifest_preflight`、`target_state_preflight`、`frame_preflight`、
`migration_processing`、`state_commit` stage。error 返回的 TargetState 必须与 API 第四
参数逐项相同且 Results 必须是 `[]`。按 callbacks key 的 Erlang term order 报第一个
error。reason detail 可以带定位 key/offset/MFA/ABI，但不得复制完整 State、Env 或
callbacks map。

ordinary callback、non-stable continuation descriptor和 accumulator values 不需要出现在 Results。若至少一个 stable entry 的
frame 被 migrated/refreshed/aborted，返回 `ok`；若所有 stable entries 都未
处理，返回 `unhandled`。

`DirectionVsn = SourceVsn | {down, TargetVsn}`，完全遵循 OTP `code_change/3`：upgrade
普通 Vsn 是 source old version；downgrade tuple 中 Vsn 是 target old version。dynamic
callback upgrade 在较新 module 加载后执行，downgrade 在旧 module 加载前由较新 module
执行。

## 6. Runtime Algorithm

enumeration MUST include every complete
`#sf_callback{frame,acc_ref,parent_ref,timer_ref}` value regardless of whether its key
is an external ActionMRef or internal ContinuationRef。ordinary `#callback{cc,acc_ref,parent_ref,...}`、
ordinary/outer descriptor nodes和accumulator data保持不变。

开始写 State 前必须验证：

- offset getter/setter 可用；
- module 导出的两个 manifest 格式、protocol 和 module 一致；
- up plan 的 current target 存在且 MFA/ABI 符合 schema；
- down plan 的 current source 存在，且每个 pending stable node 有完整 historical target
  Block/StepName/MFA/ABI schema 或可用的显式 failure policy；
- API 第四参数已采用 target State layout，TargetOffset 可读写其中的 callbacks map。

upgrade deterministic enumeration 中的每个 stable node使用 partial record pattern
`#sf_callback{frame = SourceFrame, parent_ref = ParentRef}`；该写法只选择迁移所需字段，不代表 record 只有
两个字段。完整 entry 还包含 `acc_ref`、`timer_ref` 及 shared header
扩展的 resource metadata：

1. 首先用 current manifest 检查 source Protocol/BlockName/StepName/MFA。若目标仍有相同identity，且
   resolved source version 存在该方向的 same-identity Env-only plan，则调用其 `migrate_env`，只以
   TargetEnv 重建相同 identity/MFA 的 frame；若无该 plan，则属于 `sf-do` automatic path，Env 原样；
2. 否则从 SourceFrame 读取 historical BlockName、OldStep、OldAbi；
3. 通过 `sources` 将 historical BlockName 解析到 current BlockName，再查找 config；
   没有则 unhandled；
4. exact lookup SourceVsn anchor；没有则 unhandled；
5. anchor 为 `current` 时，对 current manifest验证Protocol/Block/StepName，使用manifest MFA重建
   frame，Env 不变；该路径不查询或要求 historical step mapping，`steps => []` 也必须工作；
6. historical anchor 时，验证 OldStep/OldAbi 属于该 historical block；
7. 查找 up plan `{HistoricalBlock, OldStep}`；
8. ABI 相同且无 up migrator 时复用 Env，否则按 effective up_state 构造 context 并调用
   migrator；
9. 从 current manifest 读取 target Block/StepName/MFA/ABI，构造并验证 NewFrame；
10. 只更新 node 的 `frame` field；`ParentRef` bit-for-bit 保留。

downgrade deterministic enumeration 对同一完整 stable record使用相同 partial field projection：

1. `{down, TargetVsn}` exact lookup historical target anchor；未锚定时 preflight error；
2. 从 SourceFrame 读取 current BlockName、NewStep、NewAbi，并对 current manifest 验证；
3. 若 historical endpoint 的Protocol/Block/StepName/MFA与source identity相同，且存在该方向的
   same-identity Env-only plan，则调用其 `migrate_env` 并保持 identity/MFA；若无该 plan，该 entry
   是 automatic inheritance，Env 原样；
4. 否则查找 down plan `{HistoricalBlock, NewStep}`；没有 plan 时进入显式 mismatch policy，
   未配置 policy 则 preflight error；
5. ABI 相同且无 down migrator 时复用 Env，否则按 effective down_state 构造 context 并调用；
6. 从 historical metadata 读取 OldBlockName、OldStep、OldMFA、OldAbi；
7. 构造 old/historical frame，但不调用 OldMFA；
8. 只更新 node 的 `frame` field；`ParentRef` bit-for-bit 保留。

以下 field 必须 bit-for-bit 保留：`parent_ref`、`acc_ref`、`timer_ref` 和其他 request/resource metadata。
map key、monitor 和已发 request 不变。runtime 不得递归改写 callback value中的父链；
outer stable frame作为独立 node由同一次 enumeration处理。

## 7. Env Migrator

frame Env 必须是 `#{CaptureNameAtom => Value}` map。CaptureAbi 直接是排序后的 CaptureNameAtom
list，不得另做 SHA/digest。每个 direction 独立选择 migrator，
其输入为：

```erlang
M:F(#{env := SourceEnv} = MigrationContext) ->
    {ok, TargetEnv} | {error, Reason}.
```

API 入口必须在任何 working-state mutation 前保存
`StatePassedToMigrateSfCallbacks = State`。当前方向没有 effective State capture 时，
MigrationContext 必须精确为 `#{env => SourceEnv}`，不得包含 State fields。

plan 包含该方向归一化后的 `source_state_capture => StateCaptureName` 时，MigrationContext
必须额外包含：

```erlang
#{
    env => SourceEnv,
    source_state => maps:get(StateCaptureName, SourceEnv),
    current_state => StatePassedToMigrateSfCallbacks
}.
```

up 使用 effective up_state 从 historical/old SourceEnv 提取 source_state；down 使用
effective down_state 从 current/new SourceEnv 提取。`current_state` 必须是传给
`migrate_sf_callbacks/4` 的第四个参数原值，而不是 callbacks-cleared LogicalState、working
State 或 mismatch callback 更新后的 State；同次调用的所有 migrator 使用同一个值。

runtime 只自动提供 context，不自动修改、升级或写回其中任一 State，也不隐式把
`current_state` 放入 TargetEnv。若 runtime 收到缺失 StateCaptureName 的畸形 SourceEnv，则进入
`env_migration_failed` mismatch。runtime catch migrator 的 `error`/`exit`/`throw` 并转成
同一 mismatch。TargetEnv 必须是 capture-name atom map；migrator 不得直接修改 callbacks map。

anonymous/local fun SHOULD NOT 出现在 Env。base implementation MAY 继续以 `unstable_capture`
拒绝包含 local fun 的 TargetEnv；当前 implementation gate 不要求识别或重新生成它。optional
follow-up 若支持 fun-valued capture，MUST 只接受 explicit migrator 在 target code 中重新生成的 fun，
并由应用在每次可能 purge defining code 的 upgrade/downgrade 重复迁移。不得自动复用 SourceEnv
中的旧 fun，也不得以 source body 未变化作为兼容证据。

ABI 不同且当前方向 `migrate_env => undefined` 时同样进入 runtime
`capture_abi_mismatch`；compiler 不得提前拒绝该 declaration。
显式 migrator 在 ABI 相同或不同、boundary identity相同或不同的plan上都必须执行；same-identity plan
只替换 Env，Protocol/BlockName/StepName/MFA 必须保持 bit-for-bit 不变。

## 8. Mismatch Callback and Forced Abort

```erlang
M:F(Info, LogicalState, ParentRef) ->
    {abort, NewLogicalState}.
```

Info 使用与 result 相同的 endpoint 语义：

```erlang
#{
    module => Module,
    direction => up | down,
    version => SourceVsn | TargetVsn,
    anchor => HistoricalBlock | undefined,
    source_block => SourceBlock,
    source_step => SourceStep,
    source_capture_abi => SourceAbi,
    target_block => TargetBlock | undefined,
    target_step => TargetStep | undefined,
    callback_ref => Ref,
    reason => mismatch_reason()
}.
```

`mismatch_reason()` 固定为 `historical_async_not_found | step_not_mapped |
capture_abi_mismatch | env_migration_failed | unstable_capture |
current_async_not_found | current_capture_abi_mismatch |
unanchored_downgrade_target | invalid_historical_target`。

runtime 向 callback 提供 callbacks field 已清空的 logical State；返回后用
`async_r_t` 同等的 merge 规则把 working callback map 放回 State。callback 不得通过
State 直接增删 callback entries。downgrade callback 收到的是应用传入的 target historical
layout LogicalState，不是 source current layout。

callback 的任何异常或除 `{abort, NewLogicalState}` 外的返回都转为 forced abort。callback
不得构造 replacement frame；新 frame 只能来自 manifest 中的 explicit migration plan。

upgrade 未配置 callback 时沿用 forced abort。downgrade 未配置 callback 时，missing down
plan、未锚定 target 或无效 historical target 必须返回 error，阻止 OTP 加载旧 module；
只有显式 callback 返回 abort，才允许删除该 pending operation 后继续 downgrade。

forced abort：

- 从 working map 删除 entry；
- cancel explicit timer；
- demonitor reference when applicable；
- 通过 original direct `parent_ref` dispatch
  `{error, {async_aborted, {sf_migration, Info}}}`；
- 不调用 OldFrame、NewFrame或ordinary action `cc`；linked chain中的显式 failure handler MAY 消费 Outcome，
  普通 success frame MUST 跳过。

## 9. State Commit

manifest/target-offset 基础验证失败时不得修改 State。downgrade 还必须在 mutation 前预检
target anchor、所有 current source frames、down plans 和 historical target metadata。
通过预检后，runtime 在 working map 上逐项处理，并在结束时通过 target callbacks setter
一次提交。

per-entry mismatch 是业务配置处理结果，不回滚其他成功 entry。mismatch callback 的
NewLogicalState 按 deterministic callbacks key order 串行传递。文档和测试必须固定该
排序，避免 map iteration order 影响 State。

failure dispatch 在对应 entry 已从 working map 移除后执行。dispatch 结果记录在 aborted result 的
`reason.dispatch`，失败不得把 stale frame 放回 callbacks。

如果 Env migrator/policy 的 runtime failure 升级为 API-level error，必须丢弃全部 working
State 和 results，返回入口 TargetState 与 `[]`；timer/monitor cancellation、completion
failure dispatch 和 frame commit 必须延迟到确认整个调用可提交之后。用户 migrator/policy
自己的外部副作用不在 rollback 保证内，文档应要求它们保持 side-effect-free。

downgrade 成功返回后，callback map 中不得残留属于 current/new module 的 stable frame；
每个 pending frame 必须已变成 historical target frame或已由显式 policy abort。任何
preflight error 都必须使 `code_change/3` 返回 error，从而利用 OTP 对 dynamic callback
downgrade“先 code_change、后加载旧 module”的顺序阻止代码回退。

## 10. Required Tests

- v1/v2 -> block_1、v3 -> block_2、v4 -> current；
- exact version term，无 atom/binary coercion；
- conflicting anchors 编译失败；
- full historical block 只生成 metadata，不生成 executable functions；
- old/new `{step,StepName}` 分别在 historical/current block 精确解析；缺失 step 编译失败；
- index、bind name、content 和 Offset selector 均作为 unsupported syntax 编译失败；
- omitted mapping 对不同Block/Step identity导致`step_not_mapped`，不会fuzzy match；相同
  Protocol/Block/StepName属于`sf-do` automatic path，不是migration identity-map；
- upgrade unanchored source 保持 unhandled；downgrade unanchored target 阻止 code change；
- up/down direction allowlist、same ABI reuse，以及独立的双向 Env migrator；
- Env 是 capture-name map，普通 migrator 只收到 `#{env => SourceEnv}`；
- declaration/step `state` shorthand、up_state/down_state default/conflict 和 capture validation；
- state-aware migrator 的 `source_state` 来自该方向 SourceEnv，`current_state` 精确等于
  API 第四参数；
- 多 entry 与 mismatch State 更新不改变 Env migrator 看到的 current_state snapshot；
- 未声明方向 state 时 context 不暴露 source_state/current_state，也不按 capture name 自动识别；
- runtime 不自动升级/降级 State 或把 current_state 写入 TargetEnv；
- migrator failure 进入 mismatch；base policy 可将 local-fun result 归为 `unstable_capture`；
- successful migration only changes each stable node frame and preserves direct parent_ref；
- current anchor refreshes frame from current manifest；
- current anchor 在 `steps => []` 时仍独立 refresh，不查询 historical mapping；
- mismatch abort、非法返回和 callback crash；
- mixed callbacks map 中 ordinary callback、non-stable descriptor与accumulator entries bit-for-bit 不变；
- pending request 不重放，原 reply 进入 new frame；
- upgrade purge old module 后不调用 historical code；
- OTP-style downgrade 在 v4 code_change 中构造 v3 frame，旧 module 加载前不调用 v3 MFA；
- downgrade 使用 target State layout/offset，missing down plan 或无效 historical target
  不部分提交并阻止旧 module 加载；
- 匿名 `{F,A,N}` ↔命名 block 的双向 plans；
- 未调用 API 时零 migration side effect。
- 仅含 `sf_do` 时不生成 migration manifest/export，用户定义同名 manifest function 时
  编译期 collision diagnostic；
- public results 固定 source/target fields，API errors 固定 stage/context、入口 State 和空 Results；
- README 覆盖 declaration/options、selectors、upgrade/downgrade、State context、result/error
  以及 appup/downgrade 最小示例；
- `rebar3 dialyzer` 不新增 feature-specific warning；不得用宽泛 suppress 掩盖问题。
