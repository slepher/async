# sf-migrate Specification

## ADDED Requirements

### Requirement: Automatic identity inheritance is the default

目标 current manifest 中相同的 pending AsyncId/MFA MUST 默认自动继承。
derived identity MUST 使用 protocol、
BlockName、零基 AsyncIndex 和 scope-aware canonical AsyncForm；named identity MUST 使用
protocol、BlockName 和 StepName。没有显式 Env-only plan 时，该路径 MUST NOT 要求 `sf_migrate`
declaration、version anchor、step mapping 或 `migrate_sf_callbacks/4`。用户显式调用 migration
API 且当前方向存在 same-identity `migrate_env` 时，runtime MUST 在 resume 前转换 Env，同时保持
AsyncId/MFA。该 async 之后的
continuation、bind pattern、CaptureAbi 和 Env values MUST NOT 参与 automatic identity。

#### Scenario: Same async automatically resumes new continuation

- **给定**old/new block 的 AsyncIndex 1 都是同一 canonical `fetch_order(User)`
- **并且**新版本完全重写该 action 之后的 continuation/captures
- **当**old pending reply 在 code change 与 old-code purge 后到达
- **那么**runtime 使用相同 AsyncId/MFA 进入新 continuation
- **并且**不查询 migration manifest

#### Scenario: Module has no migration declaration

- **给定**module 只包含 `sf_do` 且 pending AsyncId 在目标 manifest 中仍存在
- **当**upgrade 或 downgrade 后 reply 到达
- **那么**frame 仍自动继承

### Requirement: Unmatched async fails closed

runtime MUST 在 source AsyncId 不存在于目标 manifest 时 fail closed。触发条件包括 derived
identity 的 BlockName、AsyncIndex 或 AsyncForm 变化，named identity 的 BlockName 或
StepName 变化，以及 derived/named mode 切换。runtime MUST NOT 使用相邻 index、
AST/capture 相似度、bind name 或 runtime value 猜测
target。若 `sf_migrate.steps` 未显式将 source selector 映射到 target selector，或
application 未在 `code_change/3` 调用 migration API，frame MUST 默认失败。

#### Scenario: Async index shifts without mapping

- **给定**old pending action 之前插入新 async，使它的 AsyncIndex/AsyncId 改变
- **并且**没有执行显式 migration
- **当**pending reply 到达
- **那么**upgrade 返回 `{error, {appup, incompatible_frame}}`，downgrade 返回
  `{error, {appdown, incompatible_frame}}`

#### Scenario: Async form changes without mapping

- **给定**AsyncIndex 不变但 action 从 `fetch_order(User)` 改为 `fetch_order_v2(User)`
- **并且**无显式 old/new selector mapping
- **当**runtime 恢复 source frame
- **那么**runtime 不得因两个 action 语法相似而继承

### Requirement: Explicit complete historical block

每个 `sf_migrate` declaration MUST 接收完整 historical block 和 Spec。derived historical
block MUST 使用 `[Monad || ...]`；named historical block MUST 使用
`sf_migrate(StepNames, [Monad || ...], Spec)`，其中 StepNames 与三参数 `sf_do` 一样支持 record 名或 atom
literal list，并遵守唯一、async-only order 和 exact-count 规则。compiler MUST 使用该
block 生成 historical metadata，不得从 current block 或旧 BEAM 推断历史内容。

#### Scenario: Historical source differs from current

- **给定**v1 block 已不同于 current named `sf_do`
- **当**用户在 `sf_migrate` 中提供完整 v1 block
- **那么**compiler 从 v1 block 计算 old AsyncId 和 CaptureAbi
- **并且**不生成或调用 v1 executable stable functions

#### Scenario: Partial historical block

- **给定**declaration 只提供不能构成完整 do block 的历史片段
- **当**module 编译
- **那么**transform 报 `invalid_sf_migrate_syntax`

### Requirement: Unified bidirectional block transition

Spec 的 `transition` MUST 包含两个 `block_ref()` endpoint：命名 endpoint 使用 atom，
匿名 endpoint 使用 `{Function, Arity, N}`。transform MUST 将匿名 endpoint 解析为
`'-Function/Arity-fun-N-'`，用户 MUST NOT 填写 generated atom、AsyncId 或 hash。

upgrade 与 downgrade MUST 使用同一个 transition schema、manifest schema 和 runtime API；
不得实现两套互不相干的 runtime。`old` selector MUST 永远描述 historical/较旧 block，
`new` selector MUST 永远描述 current/较新 block：upgrade MUST 执行 old → new，downgrade
MUST 执行 new → old。编译较新版本时，两个 endpoint 中 MUST 恰有一个存在于 current
manifest，另一端由完整 historical block 描述。

dynamic callback module MUST 遵循 OTP advanced update ordering：upgrade 在较新 module
加载后执行 `code_change(SourceVsn, ...)`；downgrade 在旧 module 加载前，由仍在运行的
较新 module 执行 `code_change({down, TargetVsn}, ...)`。downgrade 成功后 OTP 才可加载
historical target module。

#### Scenario: Upgrade anonymous block to named block

- **给定**historical endpoint 为 `{handle_call, 3, 0}`
- **并且**current endpoint 为命名 block `request_flow`
- **当**appup migration 执行
- **那么**runtime 从 `'-handle_call/3-fun-0-'` 迁移到 `request_flow`

#### Scenario: Downgrade named block to anonymous block

- **给定**同一 transition 的两个 endpoint 为 `{handle_call, 3, 0}` 与 `request_flow`
- **并且**v4 current manifest 包含 `request_flow`
- **并且**v4 historical metadata 描述 v3 `'-handle_call/3-fun-0-'`
- **当**v4 执行 `code_change({down, V3}, ...)`
- **那么**runtime 从 new/current `request_flow` frame 构造 old/historical anonymous frame
- **并且**在该 code_change 成功前不得加载或调用 v3 module

#### Scenario: Ambiguous transition endpoint

- **给定**transition 的两个 endpoint 同时存在或都不存在于 current manifest
- **当**module 编译
- **那么**transform 报 `sf_transition_endpoint_mismatch`

### Requirement: Version anchors

系统 MUST 支持多个 exact version terms 锚定到同一 historical block，并支持显式
current anchor。相同 version 指向不同 block MUST 是编译错误。

#### Scenario: Shared historical block

- **给定**v1 和 v2 锚定 block_1，v3 锚定 block_2，v4 锚定 current
- **当**migration manifest 生成
- **那么**manifest 包含
  `#{v1 => block_1, v2 => block_1, v3 => block_2, v4 => current}`

#### Scenario: Version representation differs

- **给定**manifest 锚定 atom `v1`
- **当**runtime 收到 binary `<<"v1">>`
- **那么**该 version 不匹配 anchor

### Requirement: Directional handling of unanchored versions

upgrade SourceVsn 未锚定时，runtime MUST 保持对应 stable callback 原样并标记 unhandled；
不得调用 mismatch callback 或自动选择 historical block。downgrade TargetVsn 未锚定时，
runtime MUST 在修改 State 前返回 error；不得把 current/new frame 原样交给即将加载的旧
module。

#### Scenario: Unknown old version

- **给定**OldVsn 不在 block anchors 中
- **当**migration API 扫描其 pending frame
- **那么**frame 和 callback 其他 fields 均不改变
- **并且**entry result 为 unhandled

#### Scenario: Unknown downgrade target version

- **给定**`code_change({down, TargetVsn}, ...)` 的 TargetVsn 不在 historical anchors 中
- **并且**callbacks 中存在 pending current/new frame
- **当**downgrade migration preflight
- **那么**runtime 返回 error且 State 不变
- **并且**code change 不得继续加载旧 module

### Requirement: Explicit step allowlist

每个 step mapping 的 `directions` MUST 是独立的 direction allowlist。存在 `up` 表示允许
old/historical → new/current；存在 `down` 表示允许 new/current → old/historical。未开放
的方向 MUST NOT 因 index 接近、ABI 或 content 相似而自动迁移。该 allowlist
适用于 source/target AsyncId 不同的 boundary mapping，也适用于显式配置 `migrate_env` 的
same-identity Env-only mapping。没有 Env-only plan 时，相同 AsyncId 仍由 `sf-do` automatic path
继承。up/down MUST 可配置不同的 Env migrator，
runtime MUST NOT 反向调用另一个方向的 MFA。

resolved old/new AsyncId 与 runtime BlockName 都相同、且所有 enabled direction 均未配置
`migrate_env` 时，compiler MUST 报 `redundant_sf_migration_mapping`。若至少一个 enabled
direction 显式配置 migrator，compiler MUST 生成 Env-only plan；runtime MUST 保持 AsyncId/MFA，
只用 migrator 返回的 TargetEnv 更新 frame。same-identity mapping 中没有配置 migrator 的另一方向
MUST 继续 automatic inheritance，不得生成 no-op plan。

#### Scenario: Redundant mapping of an automatically inherited async

- **给定**old/new selector 解析到相同 BlockName/AsyncId
- **并且**所有 enabled direction 都没有配置 `migrate_env`
- **当**module 编译
- **那么**transform 报 `redundant_sf_migration_mapping`
- **并且**不生成 migration plan

#### Scenario: Same identity with explicit Env migration

- **给定**old/new selector 解析到相同 BlockName/AsyncId/MFA
- **并且**`directions.up.migrate_env` 显式配置了 external MFA
- **当**upgrade migration API 处理 pending frame
- **那么**runtime 调用该 MFA 生成 TargetEnv
- **并且**frame 的 BlockName、AsyncId 和 MFA 保持不变

#### Scenario: Omit one historical step

- **给定**historical block 有三个 pending-capable boundaries
- **并且**Spec 只 mapping 前两个
- **当**callback 停在第三个 step
- **那么**runtime 产生 `async_not_mapped` mismatch

#### Scenario: Downgrade direction omitted

- **给定**step mapping 只包含 `directions => #{up => #{}}`
- **当**pending new/current frame尝试 downgrade
- **那么**该 step 不允许 down migration
- **并且**runtime 不得自动反转 up plan

### Requirement: Unique source-level selectors

mapping MUST 支持 `{index, N}`、`{name, Name}`、`{content, Expression}` 以及带正向
Offset 的 `{name, Name, Offset}`、`{content, Expression, Offset}` selector。compiler MUST
为每个 historical/current block 按源码顺序建立统一的 1-based `bind|then|let` statement
index。每个 bind/then monadic action 还 MUST 拥有不计 let 的独立零基 AsyncIndex。

name/content selector MUST 在应用 Offset 前唯一定位 anchor；Offset MUST NOT 用于选择
重复匹配中的第 N 次出现。Index 和 Offset MUST 是 `>= 1` 的 integer，最终 source index
MUST 在对应 block 范围内。无 Offset 时 selector 选择 anchor 本身；有 Offset 时 target
MUST 是 `AnchorIndex + Offset`。

最终 target MUST 是 bind 或 then 对应的 continuation；let MAY 参与计数并作为 content
anchor，但 MUST NOT 作为 migration target。所有 selector 解析和合法性检查 MUST 在编译期
完成。runtime manifest MUST 只保存解析后的 AsyncId，不得在升级时重新解析 index、Offset
或 AST。

old selector MUST 始终在 historical block 解析，new selector MUST 始终在 current block
解析。downgrade MUST 交换 source/target AsyncId 的使用方向，但 MUST NOT 交换 old/new 的
版本含义。

mapping MUST 另外支持 `{step, StepName}`，直接解析 named BlockPlan 中由 StepNames 声明的
identity。该 selector MUST NOT 依赖 source index 或 action form，MUST NOT 接受 Offset；
对 derived BlockPlan 使用时 MUST 报 `sf_step_not_found`。

#### Scenario: Adopt names from a derived block

- **给定**historical block 使用 derived identity，current 三参数 `sf_do` 声明 `load_user`
- **并且**mapping 的 old 为 `{content, fetch_user(UserId)}`，new 为 `{step, load_user}`
- **当**compiler 解析 mapping
- **那么**mapping 保存不同 namespace 的 OldAsyncId 与 NewAsyncId
- **并且**runtime 可显式迁移该 pending frame

#### Scenario: Same named step needs no mapping

- **给定**old/new 都使用相同 BlockName 和 StepName `load_user`
- **并且**action index/form 已改变
- **并且**不需要转换 Env
- **当**module 编译或 pending reply 到达
- **那么**frame 由相同 named AsyncId/MFA 自动继承
- **并且**无 `migrate_env` 的显式 old/new `{step,load_user}` mapping 报
  `redundant_sf_migration_mapping`

#### Scenario: Rename a named step

- **给定**historical named block 声明 `load_user`，current named block 改为 `fetch_user`
- **并且**mapping 使用 old `{step,load_user}` 与 new `{step,fetch_user}`
- **当**compiler 解析 mapping
- **那么**两个 StepName 分别解析为 OldAsyncId 与 NewAsyncId

#### Scenario: Resolve by bind name

- **给定**historical block 只有一个 `<-` boundary 使用 selector name `'Reply2'`
- **当**compiler 解析 `{name, 'Reply2'}`
- **那么**mapping 保存该 boundary 的 OldAsyncId

#### Scenario: Resolve by direct source index

- **给定**block 的前三个 source statements 依次是 bind、let、then
- **当**compiler 解析 `{index, 3}`
- **那么**mapping 保存该 then statement 对应 continuation 的 AsyncId

#### Scenario: Resolve forward from a unique anchor

- **给定**`{content, Expression}` 在 block 中唯一匹配 source index 2
- **并且**source index 4 是 bind 或 then
- **当**compiler 解析 `{content, Expression, 2}`
- **那么**target source index 为 4
- **并且**mapping 保存 index 4 对应 continuation 的 AsyncId

#### Scenario: Ambiguous content

- **给定**同一 block 有两个 canonical action 与 content selector 相同
- **当**module 使用 `{content, Expression, 1}` 编译
- **那么**transform 报 `sf_step_ambiguous`

#### Scenario: Invalid or out-of-range selector number

- **给定**selector 的 Index/Offset 不是 `>= 1` 的 integer，或计算后的 target 超出 block
- **当**module 编译
- **那么**transform 报对应的 invalid/out-of-range selector error

#### Scenario: Let cannot be migrated

- **给定**direct index 或 anchored Offset 的最终 target 是 let statement
- **当**module 编译
- **那么**transform 报 `sf_step_not_migratable`

#### Scenario: Old and new indexes are compiled independently

- **给定**同一逻辑 step 在 historical block 与 current block 的 source index 不同
- **并且**mapping 显式提供 old selector 与 new selector
- **当**module 编译
- **那么**两侧分别解析为 OldAsyncId 与 NewAsyncId
- **并且**runtime 不受 source index 漂移影响

### Requirement: Shared compiler analysis

historical block MUST 使用 `sf-do` 相同的 boundary analyzer、async-only index、scope-aware
canonicalizer、AsyncId 和
CaptureLayout。CaptureAbi MUST 直接是排序后的 capture-name atom list；不得为 capture 维护
第二套 canonicalizer/hash。`sf-migrate` MUST 扩展同一个 `async_sf_transform` orchestrator。

#### Scenario: Current and historical cross-validation

- **给定**同 module 同时存在 `sf_do` 和 `sf_migrate`
- **当**module 编译
- **那么**transform 先建立 current plans，再解析 historical mappings
- **并且**不存在 parse-transform ordering 导致的 manifest 缺失

### Requirement: Unpublished manifest contract

directional migration manifest MUST 使用唯一的 `format => 1, protocol => 1` schema。该功能
尚未发布，旧开发草案的 flat plans MUST NOT 被视为兼容输入；compiler/runtime MUST 同时
切换到 directional schema，不得增加版本或 dual reader。

只有 module 至少包含一个 `sf_migrate` declaration 时，transform 才可生成并 export
`'$async_sf_migration_manifest'/0`。用户 module 已定义同名同 arity function 时 MUST 在
编译期报告 collision，不得覆盖或生成重复 definition。

#### Scenario: Old draft manifest is not accepted

- **给定**runtime 收到 format 1 但使用旧 flat plans shape 的 migration manifest
- **当**manifest preflight
- **那么**runtime 返回 structured `invalid_manifest` error
- **并且**不得尝试兼容解释旧草案

#### Scenario: Module without migration declaration

- **给定**module 只有 `sf_do` 且没有 `sf_migrate`
- **当**module 编译
- **那么**不得生成或 export `$async_sf_migration_manifest/0`

#### Scenario: User manifest function collision

- **给定**module 已定义 `$async_sf_migration_manifest/0`
- **并且**module 包含 `sf_migrate`
- **当**module 编译
- **那么**transform 报明确 generated-function collision error

### Requirement: Directional capture environment migration

compiler 和 runtime MUST 分别处理 `up` 与 `down` 的 CaptureAbi。source/target
CaptureAbi 相同且该方向未配置 migrator 时，runtime MUST 复用 SourceEnv；ABI 不同则
MUST 通过该方向显式配置的 external MFA 生成 TargetEnv，否则进入 mismatch。一个方向的
migrator MUST NOT 被推断为另一方向的逆变换。frame Env MUST 是 capture-name atom 到
runtime value 的 map。未声明该方向的 State capture 时，migrator context MUST 精确为
`#{env => SourceEnv}`。

CaptureAbi MUST 直接是排序后的 capture-name atom list，不得另做 SHA/digest。显式 migrator
MUST 在 ABI 相同或不同、AsyncId 相同或不同的 mapping 上都可用；这允许变量改名、capture set
变化以及同名变量 value 语义转换。same-identity plan MUST 只替换 Env，不改变 AsyncId/MFA。

ABI 不同但缺少该方向 migrator MUST NOT 是 compile error。compiler 只能分析 declaration
中的 historical metadata，不能访问真正运行中的 source frame/Env；它 MUST 生成
`migrate_env => undefined` plan，由 runtime 根据实际 frame 产生
`capture_abi_mismatch` mismatch，使显式 `on_mismatch` 有机会处理。

#### Scenario: Upgrade ABI unchanged

- **给定**mapped historical/current step 的 CaptureAbi 相同
- **并且**`directions.up` 未配置 `migrate_env`
- **当**entry 向上迁移
- **那么**target frame 原样复用 historical SourceEnv

#### Scenario: Downgrade ABI changed

- **给定**current/historical CaptureAbi 不同
- **并且**`directions.down.migrate_env` 配置了 external MFA
- **当**MFA 接收 current SourceEnv context 并返回 `{ok, TargetEnv}`
- **那么**runtime 使用 historical target metadata 和 TargetEnv 构造 target frame

#### Scenario: Reverse migrator is not inferred

- **给定**mapping 只为 `directions.up` 配置了 `migrate_env`
- **并且**down 方向的 source/target CaptureAbi 不同
- **当**尝试向下迁移
- **那么**runtime 不得反向调用 up migrator
- **并且**该 entry 进入 missing directional migration mismatch

#### Scenario: Missing directional migrator compiles

- **给定**declared source/target CaptureAbi 不同
- **并且**enabled direction 没有 `migrate_env`
- **当**module 编译
- **那么**compiler 接受 declaration并保存 undefined migrator
- **并且**实际 entry 迁移时 runtime 进入 `capture_abi_mismatch`

#### Guidance: Anonymous/local fun capture

Env 原则上不应包含 anonymous/local fun。code change 不会自动清除或替换 State/Env 中的 fun；
old code purge 后它可能不可调用。需要保留 fun-valued capture 的应用 SHOULD 使用 directional
Env migrator 在 target code 中重新生成，或迁移为 external MFA/data descriptor，并在以后每次
相关 upgrade/downgrade 重复。当前 change 不强制实现该 regeneration capability；base runtime MAY
把含 local fun 的 TargetEnv 归为 `unstable_capture`。未来若支持，MUST 只通过显式 Env migrator
引入 target-version fun，不得自动复用 SourceEnv 的旧 fun 或按源码文本相等判定兼容。

### Requirement: Directional State-aware Env context

declaration 和 step mapping MAY 声明 `state`、`up_state` 与 `down_state`。每个值 MUST
是 literal capture-name atom。`state => S` MUST 规范化为同时存在
`up_state => S` 与 `down_state => S`；同一层若还显式声明方向字段且值不同，compiler
MUST 拒绝。declaration-level directional value MUST 作为该 declaration 中所有 step 的
默认值；step MAY 省略以继承或重复相同值，但 MUST NOT 覆盖为不同值。该默认值 MUST NOT
按 old version 分支，也 MUST NOT 跨 declaration 传播。

`up_state` MUST 在 historical/old source step capture layout 中验证，`down_state` MUST
在 current/new source step capture layout 中验证。directional State capture 只在该方向
实际调用 Env migrator 时增加 context fields；same-ABI/no-migrator reuse 不调用 context。
compiler MUST NOT 根据变量名或 term shape 自动推断 gen_server State。

存在有效 directional State capture 时，migrator context MUST 包含
`#{env => SourceEnv, source_state => SourceState, current_state => PassedState}`。
`source_state` MUST 精确等于 `maps:get(EffectiveDirectionState, SourceEnv)`；
`current_state` MUST 精确等于调用 `migrate_sf_callbacks/4` 时传入的第四个 State 参数原值。
同一次 API 调用中的所有 migrator MUST 看到相同 current_state snapshot。runtime MUST
NOT 为构造 current_state 而清空 callbacks、执行 appup/appdown、使用 working State 或
使用 mismatch callback 返回的 State。

这些字段 MUST 只提供 migration context。runtime MUST NOT 自动升级或降级 State，也
MUST NOT 自动把 current_state 写入 TargetEnv。migrator 负责决定 TargetEnv；只有其返回的
TargetEnv 才按当前 Env validation policy 验证并进入 target frame。optional local-fun regeneration
support 可在后续 change 中定义，不属于本 requirement 的强制验收。

#### Scenario: State shorthand enables both directions

- **给定**declaration 或 step 声明 `state => 'State'`
- **当**compiler 规范化 mapping
- **那么**有效配置同时包含 `up_state => 'State'` 与 `down_state => 'State'`

#### Scenario: Directional State captures may differ

- **给定**mapping 声明 `up_state => 'StateV3'` 与 `down_state => 'StateV4'`
- **当**compiler 分别解析两个方向
- **那么**up 在 historical source layout 验证 `'StateV3'`
- **并且**down 在 current source layout 验证 `'StateV4'`

#### Scenario: Declaration State default is inherited

- **给定**declaration 声明 `up_state => 'State'`
- **并且**其中一个 step 省略 up_state，另一个 step 重复 `up_state => 'State'`
- **当**module 编译
- **那么**两个 step 的有效 up_state 均为 `'State'`

#### Scenario: Step conflicts with declaration State default

- **给定**declaration 声明 `down_state => 'StateV4'`
- **并且**step 声明 `down_state => 'OtherState'`
- **当**module 编译
- **那么**transform 拒绝该 declaration

#### Scenario: Upgrade State-aware context uses historical source

- **给定**up_state 为 `'StateV3'` 且 historical SourceEnv 包含 captured v3 State
- **并且**API 第四参数是已业务转换为 v4 layout 的 `PassedStateV4`
- **当**runtime 调用 up Env migrator
- **那么**source_state 是 captured v3 State
- **并且**current_state 是 `PassedStateV4`

#### Scenario: Downgrade State-aware context uses current source

- **给定**down_state 为 `'StateV4'` 且 current SourceEnv 包含 captured v4 State
- **并且**API 第四参数是已业务转换为 v3 layout 的 `PassedStateV3`
- **当**runtime 调用 down Env migrator
- **那么**source_state 是 captured v4 State
- **并且**current_state 是 `PassedStateV3`

#### Scenario: State directive is absent for a direction

- **给定**该方向没有有效 state field
- **即使**SourceEnv 包含名为 `'State'` 的 capture
- **当**runtime 调用该方向的 Env migrator
- **那么**context 精确为 `#{env => SourceEnv}`
- **并且**不包含 `source_state` 或 `current_state`

#### Scenario: Invalid directional State capture

- **给定**directional State value 不是 literal atom、未出现在该方向 source capture layout，
  或运行时 SourceEnv 缺少对应 key
- **当**module 编译或 runtime 验证 context
- **那么**对应阶段拒绝 migration declaration 或将 entry 归为 env migration failure

#### Scenario: State is not implicitly reinserted

- **给定**state-aware migrator 收到 current_state
- **当**migrator 返回 `{ok, TargetEnv}`
- **那么**runtime 只使用该 TargetEnv
- **并且**不得自动把 current_state 添加或覆盖到 TargetEnv

### Requirement: Replace only the pending frame

成功迁移 MUST 只替换 `#sf_callback.frame`。request key、acc_ref、timer、monitor、
completion target 和已发出的 request MUST 保持不变。

#### Scenario: Resume from current breakpoint

- **给定**workflow 已完成前半段并正在等待 Ref2
- **当**其 frame 成功迁移
- **那么**Ref2 不重建，之前 step 不重跑
- **并且**Ref2 reply 到达后调用 current target stable function

### Requirement: OTP-ordered downgrade migration

downgrade MUST 在较新/current dynamic callback module 仍加载时，由该 module 的
`code_change({down, TargetVsn}, State, Extra)` 调用 migration API。应用 MUST 先把业务
State 转换为 target historical layout、保留 callbacks map，并把 target layout 的 callbacks
offset 与转换后的 State 传给 `migrate_sf_callbacks/4`。runtime MUST 在任何 mutation 前
预检 target anchor、所有 current source frames、down plans、historical target
Block/AsyncId/MFA/CaptureAbi metadata 和 target offset。

runtime MUST 只根据 historical metadata 构造 target frame，不得执行尚未加载的 historical
MFA。成功返回时，每个 pending current/new frame MUST 已迁移为 historical target frame，
或已由显式 mismatch policy terminal-abort。任何无法安全处理的 pending frame MUST 使
code change 返回 error，从而阻止 OTP 加载旧 module。

#### Scenario: Downgrade uses the newer module before old code is loaded

- **给定**v4 正在运行且 OTP 请求 downgrade 到 v3
- **当**OTP 调用 v4 `code_change({down, V3}, StateV4, Extra)`
- **那么**v4 代码完成 business State downgrade 和 stable frame migration
- **并且**只有成功返回后 OTP 才加载 v3 code

#### Scenario: Downgrade uses target State offset

- **给定**v4 与 v3 State tuple 的 callbacks field ordinal 不同
- **并且**应用已生成 target v3 State
- **当**调用 migration API
- **那么**传入并使用 v3 target offset
- **并且**runtime 不得使用 v4 record 编译出的 offset 访问 v3 State

#### Scenario: Historical target MFA is not executed early

- **给定**down plan 包含由 v3 historical block 编译得到的 target MFA metadata
- **当**v4 runtime 构造 v3 target frame
- **那么**MFA 仅作为 frame data 写入
- **并且**在 v3 code 加载并收到 pending reply 前不得调用该 MFA

#### Scenario: Missing downgrade plan blocks code rollback

- **给定**pending current/new frame 没有 down plan
- **并且**没有显式 mismatch policy 可 terminal-abort 该 entry
- **当**downgrade preflight
- **那么**migration 返回 error且 State、timer、monitor 和 completion target 均未改变
- **并且**OTP 不得继续加载 old module

### Requirement: Current anchor refresh

upgrade SourceVsn 锚定 `current` 时，runtime MUST 直接用 current manifest 验证
AsyncId 并刷新 frame MFA；不得选择 historical plan。CaptureAbi 不参与是否为
current async 的判断。downgrade TargetVsn MUST
锚定 historical block，不能把 `current` anchor 当作 old target。

#### Scenario: Frame already matches current block

- **给定**v4 anchor 为 current，pending frame 的 AsyncId 在 current manifest 中
- **当**执行 migration
- **那么**frame 从 current manifest 重建并标记 already_current
- **并且**Env 与 callback lifecycle fields 保持不变

#### Scenario: Current anchor does not require historical steps

- **给定**upgrade SourceVsn 锚定 `current`
- **并且**相关 declaration 的 `steps` 是空列表或没有匹配 source async 的 historical mapping
- **当**pending frame 的 AsyncId 存在于 current manifest
- **那么**runtime 仍直接 refresh current frame
- **并且**不得查询或要求 historical async plan

### Requirement: Stable result and error contract

每个 committed、aborted 或 unhandled stable entry MUST 返回：

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
                  notification => delivered | {failed, term()}
              }
}
```

source fields MUST 始终描述 API 入口 frame，target fields MUST 始终描述 committed/intended
frame；无法解析 target 时 MUST 使用 `undefined`。runtime MUST NOT 使用语义随路径变化的
单一 `block` field。`mapped`、`current_anchor`、`no_declaration`、
`unanchored_upgrade_source` 和包含 mismatch/policy/notification 的 abort reason MUST 使用
design/implementation contract 固定的 shape。

API-level error MUST 返回 `{error, ErrorInfo, TargetState, []}`。ErrorInfo MUST 使用：

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
}
```

TargetState MUST 与第四参数逐项相同，Results MUST 为空；reason detail MUST NOT 复制完整
State、Env 或 callback map。多个错误 MUST 按 callbacks key 的 Erlang term order 确定返回
第一个。

#### Scenario: Downgrade result names both endpoints

- **给定**current block `request_flow` 的 NewAsync 成功 downgrade 到 historical anonymous block
- **当**runtime 返回 migrated result
- **那么**source_block/source_async 描述 current `request_flow`/NewAsync
- **并且**target_block/target_async 描述 historical anonymous block/OldAsync

#### Scenario: API error returns no apparent committed result

- **给定**downgrade 在 entry processing 中遇到不能由 policy 处理的 error
- **当**API 返回 error
- **那么**State 精确等于入口 TargetState且 Results 为 `[]`
- **并且**ErrorInfo 标明 stage、reason 和第一个失败 entry context

### Requirement: Terminal mismatch callback

系统 MUST 支持 external `on_mismatch/3`，其返回仅允许 `{abort, NewState}`。
mismatch MUST NOT 进入 ordinary workflow error、catch 或 retry。

Info MUST 使用与 result 相同的 source/target endpoint fields，并包含 module、direction、
version、anchor、source_capture_abi、callback_ref 与 mismatch reason。downgrade callback
收到的 LogicalState MUST 是 API 第四参数采用的 target historical layout，不得回退为 source
current layout。mismatch reason MUST 是 `historical_async_not_found`、`async_not_mapped`、
`capture_abi_mismatch`、`env_migration_failed`、`unstable_capture`、
`current_async_not_found`、`current_capture_abi_mismatch`、
`unanchored_downgrade_target` 或 `invalid_historical_target`。

upgrade 未配置 callback 时 MUST 使用 forced-abort fallback。downgrade 中，缺少 target
anchor/down plan/historical target 且未配置显式 callback 时 MUST 返回 error；只有显式
policy abort pending entry 后，该 downgrade 才可继续。callback 抛异常或返回非法值时
MUST 使用 forced-abort fallback，但 MUST NOT 构造 replacement frame。

#### Scenario: Abort a missing mapping

- **给定**pending historical step 没有 mapping
- **并且**mismatch callback 返回 `{abort, NewState}`
- **当**migration 执行
- **那么**entry 被删除，timer/monitor 被清理
- **并且**completion target 收到
  `{error, {async_aborted, {sf_migration, Info}}}`

#### Scenario: Invalid mismatch callback result

- **给定**mismatch callback 抛异常或返回 `{abort, NewState}` 以外的值
- **当**runtime 处理结果
- **那么**entry 使用 forced-abort fallback
- **并且**callback 不得提供 replacement frame

#### Scenario: Downgrade mismatch has no implicit abort policy

- **给定**pending current frame 没有可用 down plan
- **并且**declaration 未配置 `on_mismatch`
- **当**downgrade preflight
- **那么**migration 返回 error且不修改 State
- **并且**不得沿用 upgrade 的 implicit forced-abort default

### Requirement: Shared callbacks field and legacy isolation

migration MUST 只处理现有 callbacks field 中的 `#sf_callback{}`。legacy
`#callback{}` 和 accumulator values MUST bit-for-bit 保持不变。

#### Scenario: Mixed callbacks during code change

- **给定**callbacks map 同时包含 legacy、stable 和 accumulator entries
- **当**执行 migration API
- **那么**只读取和可能修改 stable entries
- **并且**不要求第二个 callbacks offset 或 `async_sf_t`

### Requirement: Explicit code-change activation

migration plan/Env migrator MUST 仅在用户调用 `async_m:migrate_sf_callbacks/4` 时执行。
仅声明 `sf_migrate` 或加载新 async runtime MUST NOT 自动扫描 callbacks。这不限制
`sf-do` 在 reply 恢复时按 AsyncId/MFA 执行 automatic inheritance；后者不是 migration。

#### Scenario: Application omits migration call

- **给定**module 有 migration manifest
- **并且**业务 `code_change/3` 没有调用 migration API
- **当**upgrade 或 downgrade 发生
- **那么**runtime 不执行任何显式 mapping 或 Env migrator
- **并且**相同 AsyncId 仍可在 reply 到达时自动继承，不同 AsyncId 则默认失败

### Requirement: Deterministic state update

runtime MUST 在 manifest/target-offset preflight 后按确定顺序处理 stable entries，并通过
target callbacks setter 一次提交 working map。单项可处理 mismatch MUST NOT 回滚其他成功
entry。downgrade MUST 在 mutation 前预检所有 source frame、down plan 与 historical target；
基础设施或不可处理的 down mismatch MUST 保持 State、timer、monitor 和 notification 不变。

#### Scenario: Multiple pending stable callbacks

- **给定**多个 stable entries 且 mismatch callback 会更新业务 State
- **当**migration 执行多次于相同输入
- **那么**callback 调用顺序和最终 State 一致

#### Scenario: Downgrade preflight is atomic

- **给定**多个 pending current frame 中最后一个缺少合法 historical target
- **当**downgrade preflight 扫描全部 entries
- **那么**migration 在处理第一个 entry 前返回 error
- **并且**不得提交部分 frame、取消 timer 或发送 terminal notification

#### Scenario: Runtime migration failure is not partially committed

- **给定**较早 entry 已在 working data 中成功迁移
- **并且**后续 Env migrator failure 无显式 policy 可处理
- **当**runtime 将其升级为 API-level error
- **那么**丢弃全部 working State/results并返回入口 TargetState与空 Results
- **并且**不得取消 timer/monitor、发送 completion 或提交任何 frame

### Requirement: Additive compatibility

`sf-migrate` MUST 是新增显式能力。普通 do、legacy callback、未声明 migration 的
stable block 和 `async_r_t` State/store semantics MUST 保持不变。

#### Scenario: Existing process has no migration declarations

- **给定**现有进程只使用 legacy async 或未迁移的 stable workflow
- **当**升级包含 sf-migrate 的 async 版本
- **那么**进程不需要修改 State layout
- **并且**除显式 migration API 外不存在新的运行时副作用
