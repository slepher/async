# sf-migrate Specification

## ADDED Requirements

### Requirement: sf-do automatic inheritance remains authoritative

runtime MUST continue to treat named frame identity as `{Protocol, Block, StepName}`. Module Vsn,
Action AST、source index、continuation structure、capture values和app Vsn MUST NOT进入identity。
Vsn mismatch本身MUST NOT使callback失败。

当current manifest含相同named identity/current generated MFA，且current Step所需capture names是
SavedEnv keys的子集时，frame MUST 可不经sf-migrate直接恢复。SavedEnv extra keys MUST允许。

#### Scenario: Module Vsn changes but block does not

- **给定**A与B具有相同Protocol/Block/StepName
- **并且**B target required captures均存在于A SavedEnv
- **当**A frame在B current module上收到reply
- **那么**runtime直接执行B Step
- **并且**不因A与B module Vsn不同而failure

#### Scenario: No migration declaration

- **给定**FrameOwner没有`sf_do_code_change/2`
- **并且**stored named frame与current manifest满足identity和Env subset规则
- **当**callback执行
- **那么**行为与sf-do现有automatic resolution相同

### Requirement: FrameOwner owns migration declarations

定义相关`sf_do`的FrameOwner module MUST 同时拥有`sf_do_code_change/2`与其中的
`sf_migrate/3` declarations。StateHolder module MUST NOT因为保存frame而被要求复制declaration、知道
FrameOwner数量或迁移其业务State。

migration-enabled FrameOwner MUST声明显式module `-vsn`。frame和declaration MUST使用literal module
Vsn term；系统MUST NOT使用app Vsn替换它，也MUST NOT在atom、binary、string或integer之间转换。

#### Scenario: StateHolder differs from FrameOwner

- **给定**`order_server`保存callback而`order_flow`定义`sf_do`
- **当**pending frame需要显式up bridge
- **那么**runtime调用current `order_flow:sf_do_code_change/2`
- **并且**`order_server:code_change/3`不扫描或转换该frame

#### Scenario: Explicit Vsn is missing

- **给定**module定义`sf_migrate/3` special form但没有显式`-vsn`
- **当**parse transform运行
- **那么**编译以`sf_migrate_requires_explicit_vsn`失败

### Requirement: sf_do_code_change routes exact source versions and blocks

FrameOwner MUST可定义普通Erlang `sf_do_code_change(SourceVsn, SourceBlock)`，使用clauses、guards或
`case`选择一个`sf_migrate/3`结果；不支持的source MUST返回`unhandled`。transform MUST在function
存在时自动export `sf_do_code_change/2`。

每个`sf_migrate` MUST使用：

```erlang
sf_migrate(
    OldModuleVsn,
    CompleteOldComprehension,
    #{from => OldBlock, to => CurrentBlock, steps => StepMappings}
)
```

`OldModuleVsn` MUST是exact literal term。每个`{OldModuleVsn, OldBlock}` MUST至多有一个
declaration。多个历史版本MUST通过多个exact declarations表达；runtime MUST NOT推导version graph、
predecessor、version ordering或multi-hop composition。

#### Scenario: Router supports two historical versions

- **给定**current owner通过不同case branches为v1和v2分别返回`sf_migrate/3`
- **当**stored frame分别携带literal v1或v2
- **那么**runtime选择对应exact plan
- **并且**不得把其他term视为v1或v2

#### Scenario: Unknown source version

- **给定**`sf_do_code_change/2`对stored Vsn/Block返回`unhandled`
- **当**frame也不满足automatic compatibility
- **那么**该callback得到structured incompatible failure

### Requirement: Historical input is a complete named sf-do block

`sf_migrate/3`第二参数 MUST 是完整historical sf-do comprehension，`Spec.from` MUST 是它原本的
named Block atom。historical
qualifier、inline StepName marker、pattern、tail与capture analysis MUST复用sf-do grammar。transform
MUST从它得到old Step Outcome shape与Env capture shape，不得从old BEAM、source index、AST相似度或
current block猜测历史信息。

historical block MUST只生成metadata与bridge input，不得生成或执行old workflow continuation。
unnamed historical/current block MUST不可用于sf-migrate。

#### Scenario: Complete old block is copied into current owner

- **给定**v1 `order_flow`已不等于current block
- **当**current owner在v1 declaration中保留完整v1 `sf_do`
- **那么**transform从该block解析old StepName、Outcome shape与captures
- **并且**不生成或调用v1 Step body

#### Scenario: Partial historical block

- **给定**`sf_migrate/3`只提供不能构成合法sf-do的片段
- **当**module编译
- **那么**transform报`invalid_sf_migrate_syntax`

### Requirement: Step mappings use names only

Spec MUST使用literal map：

```erlang
#{
    from => OldBlock,
    to => CurrentBlock,
    steps => #{
        OldStep => #{
            to => CurrentStep,
            convert => {Module, Function, 2} %% optional
        }
    }
}
```

old/current selector MUST只使用inline StepName。transform MUST在old/current block各自精确解析，
并在missing Step/Block时编译失败。runtime MUST NOT按index、Offset、bind variable、Outcome value、
Action content或generated function name fuzzy match。

#### Scenario: Step is renamed

- **给定**old Step是`load_user`而current Step是`fetch_user`
- **并且**mapping显式配置`load_user => #{to => fetch_user}`
- **当**old callback执行
- **那么**runtime通过generated bridge进入current `fetch_user` Step

#### Scenario: Rename has no mapping

- **给定**old StepName在current manifest不存在
- **并且**exact plan没有该old Step mapping
- **当**callback执行
- **那么**runtime返回incompatible failure
- **并且**不得选择位置相邻的current Step

### Requirement: Converter is handwritten and converts Outcome with Env

parse transform MUST只派生old/current Outcome pattern shape、Env capture shape与target Step metadata。
业务转换 MUST由用户提供的external `convert/2`完成：

```erlang
M:F(OldOutcome, OldEnv) ->
    {ok, NewOutcome, NewEnv}
  | {error, Reason}.
```

runtime MUST验证NewOutcome符合current target Outcome shape，且NewEnv是atom-key map并包含current
target全部required captures。transform/runtime MUST NOT自动转换业务值、HolderState或app Vsn。

没有converter时，compiler MUST仅在normalized Outcome shape兼容且
`TargetRequiredCaptures ⊆ OldCaptureNames`时允许raw forwarding。无法证明时MUST要求converter。

#### Scenario: Outcome and Env both change

- **给定**old/current Step的Outcome pattern与capture names均不同
- **并且**mapping提供`convert/2`
- **当**old reply到达
- **那么**converter同时接收actual OldOutcome与OldEnv
- **并且**其validated NewOutcome/NewEnv被传给current Step

#### Scenario: Raw mapping lacks required target capture

- **给定**mapping没有converter
- **并且**current target需要old source不捕获的Env key
- **当**module编译
- **那么**transform报`sf_migration_converter_required`

#### Scenario: Same identity requests semantic conversion

- **给定**old/current Protocol/Block/StepName相同且Env keys兼容
- **并且**exact mapping仍配置converter以改变同名value语义
- **当**old callback执行
- **那么**explicit converter bridge优先于automatic inheritance

### Requirement: Migration executes as a reply-time bridge

runtime MUST在stored stable node真正收到Outcome并准备执行frame时才解析migration。成功migration
MUST调用version-specific old-interface bridge，bridge MUST直接转发到current generated Step。

sf-migrate MUST NOT替换或持久化新的`#sf_callback.frame`，不得枚举callback store、重新发送request、
重放已完成qualifier或执行historical Step body。原node的key、`parent_ref`、`acc_ref`、`timer_ref`、
monitor和request lifecycle MUST继续由sf-cc管理。

#### Scenario: Pending request survives upgrade

- **给定**old workflow已发出request并保存old frame
- **当**new FrameOwner加载后原reply到达
- **那么**runtime用old frame和actual Outcome选择bridge
- **并且**request不重发、old frame不写回、成功结果沿原direct parent_ref继续

#### Scenario: Linked frames have different owners

- **给定**linked continuation包含来自不同FrameOwner/Vsn的stable nodes
- **当**nodes依次执行
- **那么**每个node只按自身frame owner独立resolve
- **并且**StateHolder不需要owner list

### Requirement: Explicit mapping precedes automatic fallback

StoredVsn不同于CurrentVsn时，runtime MUST先请求
`Owner:sf_do_code_change(StoredVsn, StoredBlock)`。若returned exact plan显式包含StoredStep，runtime
MUST使用该bridge；bridge/plan失败时MUST NOT静默退回automatic path。

router缺失、返回`unhandled`或exact plan未映射StoredStep时，runtime MUST再尝试sf-do automatic named
identity与Env subset compatibility。两者均失败时才MUST产生incompatible failure。

#### Scenario: Unchanged step is omitted from historical plan

- **给定**declaration只映射一个changed Step
- **并且**另一个old Step保持相同named identity和兼容Env
- **当**未列出的Step callback执行
- **那么**runtime通过automatic path恢复
- **并且**不要求no-op mapping

#### Scenario: Explicit bridge is invalid

- **给定**exact plan显式映射StoredStep
- **并且**converter返回invalid target Env
- **当**callback执行
- **那么**callback得到incompatible failure
- **并且**runtime不得绕过converter直接调用同名Step

### Requirement: Downgrade has no reverse migration phase

app down MUST NOT触发sf-migrate callback-store扫描、eager frame conversion、`fail_sf_callbacks/4`、
reverse converter或target Vsn discovery。StateHolder `code_change({down,...})` MUST不因sf-migrate而
改变callback store。

FrameOwner旧版加载后，runtime MUST使用该实际current manifest执行普通reply-time resolution。
如果named identity存在且old target required captures是SavedEnv keys子集，callback MUST直接恢复；
否则MUST在实际触发时structured failure。down path MUST把Outcome原样传给target Step。

#### Scenario: Upgrade then immediate downgrade creates no new callback

- **给定**A callback在upgrade到B前已存在
- **并且**B未创建新callback就downgrade回A
- **当**原callback收到reply
- **那么**stored Vsn与current A重新匹配并正常执行
- **并且**appdown阶段没有提前fail它

#### Scenario: New callback uses unchanged block after downgrade

- **给定**B创建callback后downgrade到A
- **并且**A/B有相同named identity
- **并且**B SavedEnv包含A全部required captures
- **当**reply在A加载后到达
- **那么**A直接执行该Step并忽略extra Env fields
- **并且**不得只因为stored Vsn是B而failure

#### Scenario: New callback is incompatible with old target

- **给定**B callback删除了A所需capture或使用A不存在的StepName
- **当**FrameOwner已downgrade到A且reply到达
- **那么**该callback返回structured incompatible failure
- **并且**application downgrade本身不被阻止

### Requirement: Expected incompatibility follows sf-cc failure routing

expected migration incompatibility MUST形成：

```erlang
{error, {sf_runtime, {incompatible_frame, Info}}}
```

`Info` MUST包含有界的`module`、`stored_vsn`、`current_vsn`、`identity`与reason，MUST NOT包含
Env、Outcome、State、callback value或fun。failure MUST交给current node的direct `parent_ref`，不得定位
root/final Ref或修改其他node。

converter返回`{error,Reason}`与invalid converter output MUST是designed failure。converter自身抛出的
exception MUST继续遵循sf-cc既有user-code exception语义，不得被重分类为普通incompatibility。

#### Scenario: Converter rejects a value

- **给定**converter返回`{error, unsupported_value}`
- **当**bridge执行
- **那么**runtime产生reason为`converter_failed`的structured failure
- **并且**failure沿direct parent_ref传播

#### Scenario: Failure metadata is sanitized

- **给定**OldEnv和Outcome含敏感term
- **当**migration incompatibility形成Info
- **那么**Info不包含OldEnv、Outcome或HolderState

### Requirement: No release dependency mechanism is introduced

sf-migrate MUST NOT生成`-sf_migration_dependencies`，MUST NOT建立module dependency graph，MUST NOT
检测或禁止引用环，也MUST NOT更改OTP appup/relup update order。partial release窗口内的不兼容只影响
当时触发的callback，不得由sf-migrate强制阻止application upgrade/downgrade。

future Rebar plugin MAY做whole-project static analysis和warning，但其存在MUST NOT成为sf-migrate
runtime correctness条件。

#### Scenario: Two FrameOwners reference each other

- **给定**两个module的workflows存在循环业务引用
- **当**其中一个在partial release窗口收到reply且current target不兼容
- **那么**该callback可structured failure
- **并且**parse transform不拒绝module、runtime不阻止release

### Requirement: Additive compatibility and protocol boundary

sf-migrate MUST保持ordinary `do`、ordinary `#callback{}`、unnamed sf-do same-version resume、callback
store layout、`#sf_callback{}` encoding和current sf-do manifest contract不变。不得新增
`migrate_sf_callbacks/4`、第二个callbacks store或State layout要求。

async runtime自身无法decode另一版本`#sf_callback{}`的情况 MUST归入async release protocol compatibility，
不得由application sf-migrate伪装解决。

#### Scenario: Application does not use sf-migrate

- **给定**module只有现有sf-do/sf-cc代码
- **当**加入sf-migrate实现的async版本运行
- **那么**module无需新增State field或code_change调用
- **并且**其callback行为保持原样
