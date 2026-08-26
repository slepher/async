# sf-do Specification

## ADDED Requirements

### Requirement: sf_do preserves the Monad type

`sf_do/1` 和 `sf_do/2` MUST 返回 comprehension head 声明的原 Monad 类型。
transform MUST NOT 把 expression 替换为独立 `sf_program()` 或 instruction VM。

#### Scenario: Compose sf_do as an ordinary action

- **给定**一个 `async_m` `sf_do` expression
- **当**它作为 ordinary bind 或另一个 `sf_do` 的 Action 使用
- **那么**类型和运行语义仍是 `async_m`
- **并且**不需要 subprogram adapter

### Requirement: OTP-compatible source forms

最后一个 argument MUST 是 single-head list comprehension：

```erlang
sf_do([Monad || Qualifiers])
sf_do(BlockName, [Monad || Qualifiers])
```

BlockName MUST NOT 放入 comprehension head。
Monad head MUST 接受 atom、variable 和二元 transformer descriptor `{T, Inner}`。transform MUST
统一生成 `monad_frame:bind_frame/4`、`monad_frame:then_frame/4`，不得依据 Monad AST shape 直调
module，也不负责预检 instance export/typeclass metadata；resolved Monad 缺少 capability 时使用
typeclass dispatcher 的标准失败行为。

#### Scenario: Explicit block

- **给定**`sf_do(request_flow, [async_m || load, Action, finish()])`
- **当**module 编译
- **那么**block path 包含 `request_flow`
- **并且**expression 编译为 `bind_frame`/`then_frame` sequencing chain

#### Scenario: Anonymous block

- **给定**source function `f/1` 中的第一个 anonymous `sf_do`
- **并且**它使用 `sf_do([async_m || load, Action, finish()])`
- **当**module 编译
- **那么**local block component 为 `'-f/1-fun-0-'`
- **并且**named block 不消耗 anonymous occurrence 序号

### Requirement: Inline StepName markers

transform MUST 将紧随非最终 `bind`/`then` action 的 standalone literal atom qualifier 解释为
StepName marker。
marker MUST 是 compile-time metadata，并从 executable qualifier stream 删除。同一 BlockPath 内
StepName MUST 唯一。standalone literal atom qualifier 不具有 ordinary action 语义；缺少后继
boundary、修饰 pure let/final action、连续 marker 或任何其他 placement MUST 报 source error。
pure let 和 final action不形成当前 block frame，不需要 name。

#### Scenario: Named action moves

- **给定**两个版本使用相同 inline BlockPath/StepName
- **并且**action index/form 发生变化
- **当**计算 named identity
- **那么**AsyncId/MFA 保持不变

#### Scenario: Invalid marker

- **给定**duplicate StepName，或 literal atom marker 没有紧邻非最终 bind/then action
- **当**module 编译
- **那么**transform 报 source-position diagnostic

#### Scenario: Unnamed boundary

- **给定**一个非最终 bind/then action 前没有 inline atom marker
- **当**Pass 1 分析该 boundary
- **那么**transform 记录 source-position warning
- **并且**没有其他 error 时继续 Pass 2
- **并且**生成的 frame 可普通执行但不可 migration

### Requirement: Frame-aware Monad bind

compatible Monad MUST 提供 `monad_frame` capability。
`monad_frame:bind_frame(Action, NextMFA, FrameContext, Monad)`
MUST 返回与 Action 相同类型的 Monad，并具有 ordinary bind 的 sequencing semantics。
`monad_frame:then_frame(Action, IgnoreInputMFA, FrameContext, Monad)` MUST 表示丢弃 Action Reply 的 frame sequencing。
`monad_frame` typeclass default MUST 直接以相同参数调用 `bind_frame`；Reply MUST 由 generated
ignore-input frame 丢弃。这与 `classic_do` 用 `>>=` 和 `_` continuation lowering 无绑定 Action、
以及 Monad `>>` default 委托 `>>=` 的语义一致。

#### Scenario: Suspended bind

- **给定**Action 挂起
- **当**通过 `bind_frame` 运行
- **那么**callback 保存 NextFrame 和 downstream stable continuation data
- **并且**不保存 bind continuation closure

#### Scenario: Immediate bind

- **给定**Action 立即完成为 Value
- **当**通过 `bind_frame` 运行
- **那么**runtime invoke `NextFrame(Value)`
- **并且**运行其返回的同类型 Monad

#### Scenario: Unbound action uses then_frame

- **给定**非最终 qualifier `Action` 没有 `<-` pattern
- **当**transform 编译该 sequencing boundary
- **那么**生成 `monad_frame:then_frame(Action, IgnoreInputMFA, FrameContext, Monad)`
- **并且**default `then_frame` 以相同参数调用 `monad_frame:bind_frame`
- **并且**frame 接收但忽略 Reply，再直接返回 remainder Monad

### Requirement: Stable frame data has one encoding

`NextMFA` MUST 是 `{Module, Function, 2}`。`FrameContext` MUST
包含 protocol `1`、BlockPath、`StepName | undefined`、AsyncId 和 Env。runtime MUST 将两者合成
tagged-map `sf_frame`。
callback MUST 保存单一 tagged-map continuation chain：每个 `sf_next` 包含 frame 与 downstream，
chain MUST 以 tagged-map `sf_cc` 终止。前一版 prototype tuple/map 和两个并列 continuation 字段
MUST NOT 作为兼容格式保留。

#### Scenario: Nested downstream representation

- **给定**inner frame 在 outer frame 下运行
- **当**inner action 挂起
- **那么**callback 保存 `sf_next(inner_frame, sf_next(outer_frame, ...))` 的数据等价形式
- **并且**chain 最终以用户提供的 `sf_cc` 终止，不含 transform-generated fun

### Requirement: Generated frames return Monad values

每个 generated frame function MUST 接收 `(Input, Env)` 并直接返回 remainder Monad。它
MUST NOT 返回 `{await,...}`、`{done,...}` 或其他 interpreter instruction。

#### Scenario: Two boundaries

- **给定**包含两个 async actions 的 block
- **当**transform 编译 block
- **那么**每个 sequencing continuation 有独立 external function
- **并且**第一个 function 返回包含第二个 frame sequencing operation 的 Monad

### Requirement: classic_do scoped return and fail semantics

`sf_do` transform MUST 保留 `do` 对 block 内 local `return/1` 和 `fail/1` 的 scoped
rewrite，分别统一生成 `monad:return(Value, CurrentMonad)` 和
`monad_fail:fail(Reason, CurrentMonad)`。不得依据 tuple AST shape 直调 module。最后一个 qualifier MUST 是 Monad action：可以是
`return(A)`，也可以是其他 do Action；transform MUST 直接返回该 action，不生成 terminal frame。

除 continuation 从 fun 改为 frame、sequencing primitive 从 `>>=` 分为 `bind_frame`/`then_frame`
以及 stable identity/runtime data 外，`sf_do` MUST 以 ErLando `do` 的核心 qualifier、pattern、
final expression 和 lexical local-call lowering 为 baseline，不得另行添加 source semantics。
`classic_do` 的 tuple-AST direct-call branch 是实现细节，不属于严格对照合同。
只有 local `fail/1` 或 non-variable generator pattern 实际出现时，resolved Monad 才需要
`monad_fail`；transform MUST NOT 预检该 capability。

#### Scenario: Final local return

- **给定**`sf_do([Monad || action, Action, return(Value)])`
- **当**transform 编译 block
- **那么**`return(Value)` 按 Monad dispatch rewrite
- **并且**rewritten return action 直接作为 remainder 返回

#### Scenario: Final arbitrary Monad action

- **给定**最后一个 qualifier 是 `finish(Value)` 返回的 Monad action
- **当**transform 编译 block
- **那么**该 action 直接作为 remainder 返回
- **并且**不生成 `{done,...}` 或 terminal frame
- **并且**不消耗 StepName，也不生成 frame function 或 manifest entry
- **并且**若该 action 挂起，只保存进入它时已有的 outer frame 或顶层 `sf_cc` downstream

#### Scenario: Nested lexical Monad

- **给定**outer 和 inner `sf_do` 声明不同的 compatible Monad
- **并且**inner block 包含 local `return/1` 或 `fail/1`
- **当**transform rewrite nested block
- **那么**inner call 使用 inner Monad dispatch
- **并且**离开 inner block后 outer local call 继续使用 outer Monad dispatch

#### Scenario: Calls outside the scoped rewrite

- **给定**block 外的 local `return/1` 或 block 内的 remote `module:return/1`
- **当**transform 遍历 module
- **那么**这些 call 保持原样

#### Scenario: Invalid terminal qualifier

- **给定**最后一个 qualifier 是 `<-` generator 或 pure match
- **当**transform 编译 block
- **那么**transform 报 invalid last qualifier diagnostic

### Requirement: Dedicated stable completion interface

stable runner MUST 接收用户显式 external MFA/context `sf_cc` descriptor，并通过专有 stable
completion interface 调用。stable path MUST NOT 复用 legacy CC fun、`callback_to_cc` 或原
`run_cc` interface。`sf_cc` MUST 是 `#{kind := sf_cc, protocol := 1, mfa := {Module, Function, 2},
context := Context}`。transform-generated downstream continuation MUST 表示为 `sf_next` chain 或
`sf_cc` descriptor，不得是 transform 生成的 local bind continuation fun。该 requirement
不要求 `sf_do` 检查 Env、Action 或 context 中任意 runtime value。`sf_cc` MUST NOT 进入 do block
identity、manifest 或 migration。

#### Scenario: Universal stable completion

- **给定**用户把需升级的业务逻辑放在 `sf_do` 最后部分
- **并且**runner 使用通用 external MFA `sf_cc`
- **当**block 完成
- **那么**专有 `invoke_sf_cc` interface 只把最终 Outcome 交给该 descriptor
- **并且**不调用 legacy CC adapter，`sf_do` manifest 也不记录 `sf_cc`

### Requirement: Nested sf_do uses Monad composition

nested `sf_do` MUST 可直接作为 outer `bind_frame` Action。inner suspension MUST 保存 inner
NextFrame 与 outer downstream frame，使 inner completion value 进入 outer continuation。

#### Scenario: Nested suspension

- **给定**outer block bind 一个会挂起的 inner `sf_do`
- **当**inner reply 到达
- **那么**inner remainder 先完成
- **并且**结果随后恢复 outer generated frame

#### Scenario: Nested names

- **给定**两个不同 parent block 都包含 local name `inner`
- **当**生成 identity
- **那么**完整 BlockPath 不同且不冲突
- **并且**generated parent function name 不参与 child identity

### Requirement: classic_do pattern failure semantics

non-variable `<-` pattern mismatch MUST 返回
`monad_fail:fail(monad_badmatch, CurrentMonad)`。pure let mismatch MUST
保持普通 Erlang `badmatch`。`sf_do` MUST 在 external frame body 中用 `case Input of` 表达
`classic_do` anonymous continuation fun 的成功/fail clauses，不得为该 bind 重新生成 anonymous
continuation fun。

#### Scenario: Generator pattern shadows enclosing variable

- **给定**`Expected` 在 block 前已绑定
- **并且**qualifier 为 `{Expected, Value} <- Action`
- **当**frame 恢复 Input
- **那么**`Expected` 与 `Value` 都作为 generator pattern 的新 bindings 从 Input 取得
- **并且**不从 Env 恢复 enclosing `Expected` 形成额外 equality constraint

#### Scenario: Error outcome does not match success pattern

- **给定**pattern `{ok, Value}`
- **当**Action outcome 为 `{error, timeout}`
- **那么**generated frame 调用 `monad_fail:fail(monad_badmatch, CurrentMonad)`

### Requirement: Timeout and DOWN are ordinary action outcomes

timeout、DOWN 和 final reply MUST 传给当前 generated frame；runtime MUST NOT 因 outcome 类型
直接 terminal-abort stable workflow。message MUST NOT consume frame。

#### Scenario: Variable pattern receives timeout

- **给定**qualifier `Reply <- Action`
- **当**Action timeout
- **那么**Reply 绑定 timeout outcome
- **并且**block 继续其 source-defined next step

### Requirement: Structural runtime failure is terminal

unsupported protocol、损坏的 continuation shape 或 manifest/MFA validation failure MUST 清理 callback，
并恰好一次调用 terminal `sf_cc`，Outcome 为 `{error, {sf_runtime, Reason}}`。runtime MUST NOT invoke
未知 generated frame，也不得把结构错误伪装成 timeout、DOWN 或 ordinary Monad failure。

#### Scenario: Missing current MFA

- **给定**pending frame 的 AsyncId/MFA 无法在 current manifest 验证
- **当**reply 到达
- **那么**runtime 清理 request lifecycle 并以 `{error, {sf_runtime, Reason}}` 调用 `sf_cc` 一次
- **并且**不执行 stored MFA

### Requirement: Scope-aware minimal Env

每个 frame Env MUST 只捕获当前 continuation 实际使用且在 boundary 前 lexical-bound 的变量。
analysis MUST 包括 suffix actions/lets/guards、nested lexical scopes 和 generated dispatch 所需的
Monad value；generator pattern variables 和 Input MUST 作为新 bindings 排除。

Env key MUST 直接使用 source variable atom。scope-aware analysis 仍负责从正确 lexical binding
计算 free-variable capture set，并排除 generator pattern bindings、shadowed outer bindings 和 Input。
CaptureLayout MUST 按 atom 排序；兼容性按 variable atom 的 map membership 判断。变量改名和跨版本
同名变量语义变化不属于 `sf_do` 的自动判断责任，由 `sf-migrate` directional Env migrator 显式
生成 TargetEnv。CaptureAbi MUST 直接等于排序后的 capture-name atom list，不得另做 hash。
generated internal variables MUST hygienic。

Env value SHOULD NOT 是 anonymous/local fun。`sf_do` MUST NOT 递归检查或拒绝 capture runtime
values，也不保证 State 中已有 fun 在 upgrade 时被清除或替换；old code purge 后该 fun 可能不可调用。
应用若保留 fun-valued capture，SHOULD 使用 `sf-migrate` Env-only migrator 在 target code 中重新生成，
或迁移为 external MFA/data descriptor。当前 requirement 不强制实现 fun 检测或 migration declaration
校验。

#### Scenario: Exclude unused binding

- **给定**boundary 前存在 Used 和 Unused
- **并且**remainder 只引用 Used
- **当**构造 frame
- **那么**Env 包含 Used，不包含 Unused

#### Scenario: Reply binding is Input

- **给定**`Reply <- Action`
- **当**构造 Action 的 NextFrame
- **那么**Reply 不作为 pre-existing capture 保存
- **并且**恢复时由 Input 绑定

#### Scenario: Match-style clause captures an outer equality variable

- **给定**boundary 前已绑定 `Expected`
- **并且**remainder 的 `case`、`receive` 或 `try` clause pattern 使用 `Expected`
- **当**计算 required captures
- **那么**`Expected` 被视为 outer equality reference 并进入 Env

#### Scenario: Fun clause parameter is a fresh binding

- **给定**boundary 后存在 `fun(Expected) -> Expected end`
- **并且**boundary 前也存在同名 outer `Expected`
- **当**计算 required captures
- **那么**fun parameter shadow outer binding
- **并且**该引用本身不要求捕获 outer `Expected`

### Requirement: Transform does not predict future captures

`sf_do` MUST NOT 为未知 future versions 捕获 enclosing scope 的全部 bindings。目标版本新增
required capture 而 source Env 不含该值时，若 `sf-migrate` Env migrator 未在恢复前补齐，runtime
MUST 按 code-change direction 确定性失败。`sf_do` 不定义 migration DSL 或 TargetEnv 转换逻辑。

#### Scenario: New continuation needs absent prefix value

- **给定**source continuation 未使用 PrefixValue，source frame 未捕获它
- **并且**target continuation 新增对 PrefixValue 的依赖
- **当**pending frame 恢复且没有 migration
- **那么**runtime 不重放 prefix action
- **并且**以 incompatible environment 失败

### Requirement: Named async identity uses BlockPath and StepName

有 inline StepName 的非最终 `bind`/`then` qualifier 的 migration AsyncId MUST 只使用 protocol、
BlockPath 和 StepName：

```erlang
crypto:hash(sha256,
    term_to_binary({sf_named_async, Protocol, BlockPath, StepName})).
```

AsyncId 是 named tuple 的固定长度编码，不增加 Action 相似度判定。source-order index、Action AST、
source annotation、binder/reference name、map entry order、pattern、suffix、CaptureLayout 和 runtime
Env MUST NOT 参与 AsyncId。transform MUST NOT 为 identity 维护 Action canonicalizer。默认 Protocol
为 `1`。generated function atom MUST 使用 `'$sf_'` 加完整 AsyncId lowercase hex，不得截断。

unnamed boundary MUST 有仅用于普通执行/same-version resume 的 module-local locator/MFA，并在
current manifest 中记录 `step_name => undefined`。该 locator MUST NOT 被解释为 migration
identity；runtime 在 appup/appdown migration 时 MUST 在调用 frame MFA 前返回 directional
`incompatible_frame`。

#### Scenario: Change only remainder

- **给定**pending boundary 的 BlockPath/StepName 不变
- **并且**Action AST、source position 或后续 continuation 改写
- **当**目标版本编译
- **那么**AsyncId/MFA 保持不变

#### Scenario: Move a named action

- **给定**一个 named action 在 block 内移动，或在其前方插入另一个 async action
- **并且**该 action 的 BlockPath/StepName 保持不变
- **当**重新计算 identity
- **那么**该 action 的 AsyncId/MFA 保持不变

#### Scenario: Rename a step

- **给定**pending boundary 的 BlockPath 或 StepName 改变
- **当**重新计算 identity
- **那么**AsyncId/MFA 改变
- **并且**runtime 不按 index、Action AST 或相似度 fuzzy-match target

### Requirement: Two-layer resume compatibility

automatic resume MUST 同时满足 target manifest 中存在相同 BlockPath/StepName 及其 generated
AsyncId/MFA，以及 target required captures 是 StoredEnv keys 的子集。StoredEnv extra keys MUST
被允许。没有 StepName 的 frame在普通执行/same-version resume 中 MAY 继续，但在 appup/appdown
migration 中 MUST incompatible，runtime MUST NOT 从 local locator、index/AST 推导 target。

#### Scenario: Target needs fewer captures

- **给定**source Env 包含 target 不再使用的字段
- **当**恢复 target frame
- **那么**extra fields 不导致 incompatibility

#### Scenario: Identity matches but Env is insufficient

- **给定**AsyncId/MFA 相同
- **并且**target required capture 缺失
- **当**没有 Env migration
- **那么**frame 不执行
- **并且**upgrade 返回 `{error, {appup, incompatible_frame}}`，downgrade 返回
  `{error, {appdown, incompatible_frame}}`

#### Scenario: Unnamed frame cannot migrate

- **给定**pending frame 的 `step_name` 是 `undefined`
- **当**appup 或 appdown migration 处理该 frame
- **那么**不调用 stored/current generated MFA
- **并且**返回对应方向的 `incompatible_frame`

### Requirement: Current manifest has one public shape

transform MUST export `'$async_sf_manifest'/0`。manifest MUST 使用 format/protocol `1`，并按
BlockPath 和 AsyncId 记录 StepName、generated MFA 和排序后的 required capture keys。manifest
MUST 允许 unnamed entry 使用 `step_name => undefined`，并按该字段禁止 migration。manifest
MUST NOT 包含 AsyncForm digest、full source AST、runtime Env、`sf_cc`、migration selector 或
historical mapping。

#### Scenario: Query current boundary

- **给定**module 含有一个非最终 continuation boundary
- **当**调用 `'$async_sf_manifest'/0`
- **那么**返回该 boundary 的 current identity、MFA 和 required capture schema
- **并且**不返回 runtime 或 historical data

### Requirement: Astranaut compiler traversal

transform MUST 使用 AST-aware traversal 收集和替换 source occurrences。identity MUST 基于
Pass 1 冻结的 BlockPath/StepName；generated frame AST MUST NOT 被当作 source 再分析。第一遍 MUST
冻结 occurrence plan；第二遍 MUST 按稳定 lexical occurrence id 消费 plan，而不是重新分析 source
form、name-marker pairing、identity/capture，或依赖 AST term identity/source line 猜测 occurrence。

expected source diagnostics MUST 由 Pass 1 的 Astranaut return value 记录和累计，不得通过
`throw`/`error` 作为正常控制流。Pass 1 完成后只要存在一个 error，transform MUST 立即调用
`astranaut_return:to_compiler/1` 返回 compiler diagnostics，且 MUST NOT 初始化或运行 Pass 2。
若 Pass 1 只有 unnamed warnings，则 MUST 继续 Pass 2，并将 warnings 与 transformed forms 一并返回。

#### Scenario: Nested collection and rewrite

- **给定**nested `sf_do`
- **当**transform 扫描 module
- **那么**先记录 original lexical hierarchy和 occurrence identity
- **并且**统一分析后替换，不依赖 generated function owner

#### Scenario: Pass agreement

- **给定**第一遍记录 occurrence id 与 original-node digest
- **当**第二遍遇到对应 `sf_do`
- **那么**按相同 lexical pre-order id 取得冻结 plan
- **并且**id、顺序或 digest 不一致时报 transform ordering diagnostic

#### Scenario: Multiple Pass 1 diagnostics skip Pass 2

- **给定**module 含有两个彼此独立的 invalid `sf_do` occurrences
- **当**Pass 1 完成
- **那么**两个 source diagnostics 都被记录
- **并且**transform 通过 `astranaut_return:to_compiler/1` 返回
- **并且**Pass 2 rewrite、generated function 和 manifest generation 均未运行

#### Scenario: Pass 1 warning continues to Pass 2

- **给定**module 只有 unnamed boundary warning，没有 source error
- **当**Pass 1 完成
- **那么**Pass 2 正常生成 executable frame 和 manifest
- **并且**compiler result 同时包含 warning

### Requirement: Shared callback store and lifecycle

stable/legacy entries MUST 共用现有 callbacks field。stable frame path MUST 保持 request key、
monitor、timer、accumulator reference 和 issued request；reply/timeout/DOWN cleanup MUST 复用
existing lifecycle。

#### Scenario: Stable frame update

- **给定**pending stable callback
- **当**frame metadata 更新
- **那么**request 不重发、monitor 不重建、timeout 不重启

### Requirement: Additive compatibility

未使用 `sf_do` 的现有应用 MUST 保持 source、record、callback 和 runtime 行为兼容。

#### Scenario: Legacy callback

- **给定**ordinary async action 使用 fun CC
- **当**升级包含 sf-do capability 的 async library
- **那么**legacy path 仍保存和调用原 CC
- **并且**stable path 使用不同 `sf_cc` interface，不宣称 legacy CC 可迁移
