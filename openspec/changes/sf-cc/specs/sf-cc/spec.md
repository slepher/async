# sf-cc Specification

## ADDED Requirements

### Requirement: Stable CC uses dedicated sf-prefixed runners

系统 MUST 提供 `run_sf_cc/3`、`run_with_sf_cc/5` 和 `exec_sf_cc/5`，且 MUST NOT 改变现有 ordinary runner 合同。
现有 `run_cc/2`、`run_with_cc/4`、`exec_cc/4`、`run/4` 与 `callback_to_cc/1` MUST 保持 ordinary callback
语义。`sf_cc` runners MUST 可运行内部使用 ordinary anonymous continuation 或 stable frame continuation
的 Promise；用户不得为两种 Promise 提供不同 final completion API。

#### Scenario: Ordinary promise uses stable final completion

- **给定**一个内部 bind continuation 是 anonymous fun 的 ordinary Promise
- **并且**用户通过 `run_with_sf_cc/5` 提供 external final callback
- **当**Promise 挂起并在之后完成
- **那么**action callback 保持原 anonymous fun
- **并且**最终 Outcome 通过该 Promise 的 CcRef 交给 external final callback 一次

#### Scenario: Frame promise uses the same runner

- **给定**一个内部 continuation 是 `sf_frame` data 的 Promise
- **当**用户以相同 `run_with_sf_cc/5` API 运行它
- **那么**最终 completion 使用与 ordinary Promise 相同的 CcRef store 和 completion primitive
- **并且**runner 不要求用户判断内部 continuation kind

#### Scenario: Ordinary runner keeps its contract and adopts CcRef

- **给定**用户调用现有 `run_cc/2` 或 `run_with_cc/4`
- **当**Promise 挂起
- **那么**runtime 保持现有 callback 参数、normalization 和 observable result 行为
- **并且**该次运行创建 CcRef，挂起的 action callback 受同一 completion lifecycle 保护
- **并且**旧 API 不增加 external-fun source constraint，也不宣称 anonymous final callback 本身可跨 purge 稳定调用

### Requirement: Every runner invocation creates a CcRef

每次 public runner invocation MUST 进入统一 CcRef lifecycle。该规则 MUST 覆盖
`wait/1`、`wait_t/2`、`run_cc/2`、`run_with_cc/4`、`exec_cc/4`、`run/4` 及新的 sf-prefixed
runners。ordinary runners MUST 通过 ordinary completion descriptor 保留原 callback normalization 和正常
返回合同；wait runners MUST 使用 library-owned completion。runtime MUST NOT 要求 ordinary API 的 callback
满足 sf-cc macro 的 external-fun
source constraint。

#### Scenario: Wait preserves its observable result

- **给定**用户在升级后调用 `wait/1` 或 `wait_t/2`
- **当**Promise 挂起后完成
- **那么**调用方观察到与旧 wait 合同相同的正常结果
- **并且**挂起 action 在内部携带该次 wait 创建的 CcRef

#### Scenario: Ordinary callback facade uses the common lifecycle

- **给定**ordinary runner 的 final callback 是 anonymous adapter fun
- **当**该 runner 在升级后创建 pending action
- **那么**CcRef entry 持有 ordinary `sf_cc` completion descriptor，action callback 持有相同 CcRef
- **并且**normal completion 仍按旧 adapter 的 normalization 执行

### Requirement: Macro accepts only explicit external callback source forms

`run_sf_cc/3`、`run_with_sf_cc/5` 和 `exec_sf_cc/5` MUST 由独立 Astranaut macro provider 校验显式 external-fun source form。
该 provider MUST 分析第二个 source argument。`run_sf_cc/3` MUST 只接受 `fun Module:Function/2`；其他两个 runner
MUST 接受 `fun Module:Function/2` 与 `fun Module:Function/3`。macro MUST 拒绝 local fun、anonymous
fun、fun variable、任意 runtime expression 和不支持的 arity，并产生 source-position compiler error。
macro MUST 保留 external-fun AST 并生成普通 runtime call；不得拆成 MFA tuple。

#### Scenario: External async callback compiles

- **给定**源码调用 `run_sf_cc(Promise, fun completion:finish/2, Context)`
- **当**module 编译
- **那么**macro 生成接收相同 external-fun expression 的普通 runtime call
- **并且**不生成 `{completion, finish, 2}` 参数

#### Scenario: Direct State callback arities compile

- **给定**两个源码调用分别向 `run_with_sf_cc/5` 传递 external fun `/2` 与 `/3`
- **当**module 编译
- **那么**两个调用都成功展开

#### Scenario: Anonymous callback is rejected

- **给定**第二个参数是 `fun(Outcome, Context) -> ... end`
- **当**任一 sf-cc macro 分析调用
- **那么**compiler 报 external callback required diagnostic
- **并且**不生成 runtime call

#### Scenario: Fun variable is rejected

- **给定**变量 `Callback` 在 runtime 持有 external fun
- **当**源码把 `Callback` 作为 sf-cc macro 第二参数
- **那么**compiler 仍报 source-form error
- **因为**macro contract 要求调用点显式出现 external-fun AST

#### Scenario: Async runner rejects arity three

- **给定**`run_sf_cc/3` 的第二参数是 `fun completion:finish/3`
- **当**module 编译
- **那么**compiler 报 unsupported sf-cc arity

### Requirement: sf_cc descriptor preserves the external fun and Context

runtime MUST 将 macro 传入的 external-fun term 原样保存在唯一 tagged descriptor：

```erlang
#{
    kind := sf_cc,
    protocol := 1,
    mode := async_r_t | direct_state,
    callback := fun(),
    context := term()
}
```

Context MUST 是用户在 runner 调用时预存的 opaque term。Context MUST NOT 被解释为 `sf_do` Env，
不得自动捕获、迁移或暴露给 matcher。runtime MUST NOT 用 `fun_info` 重新执行 external/local source-origin
validation；它 MUST 用 fun arity 分派 mode，并 MAY 在 apply 前读取 external MFA 做 availability check。

#### Scenario: Context survives suspension

- **给定**用户把 `From` 作为 Context 运行一个随后挂起的 Promise
- **当**reply 到达并最终完成 Promise
- **那么**final callback 收到原 `From`
- **并且**不从 frame Env 或当前 State 重建 Context

#### Scenario: Descriptor stores a fun rather than MFA tuple

- **给定**macro 接收 `fun completion:finish/2`
- **当**runtime 创建 descriptor
- **那么**`callback` 是 external-fun term
- **并且**descriptor 不含 `mfa => {completion, finish, 2}`

### Requirement: sf_cc runner modes have distinct return contracts

`run_sf_cc/3` MUST 只接受返回 `async_r_t` 的 external callback `/2`，并返回最终的 `async_r_t`。
该 mode MUST NOT 接受 callback `/3`。`run_with_sf_cc/5` 与 `exec_sf_cc/5` 的 direct-state mode MUST 接受
callback `/2` 或 `/3`：`/2` return value MUST 被忽略；`/3` MUST 接收 CleanState 并返回 NewState。
`/3` NewState MUST 满足现有 State record tag/arity compatibility check；`async_r_t` 不是合法 `/3`
return。invalid return MUST 在 CcRef 已消费后失败。

#### Scenario: Async callback reads State from async_r_t

- **给定**`run_sf_cc/3` callback `/2` 返回一个调用 `async_r_t:get_state` 的 effect
- **当**final callback 执行
- **那么**effect 可读取 CleanState
- **并且**不需要 callback `/3`

#### Scenario: Direct callback arity two ignores return

- **给定**`run_with_sf_cc/5` callback `/2` 返回任意 term
- **当**final callback 执行完成
- **那么**该 term 不作为 NewState 或 completion result
- **并且**最终 State 保持 CleanState

#### Scenario: Direct callback arity three replaces State

- **给定**`run_with_sf_cc/5` callback `/3` 根据 CleanState 返回 compatible NewState
- **当**final callback 执行完成
- **那么**runner 返回/提交 NewState

#### Scenario: Direct arity three returns async_r_t

- **给定**direct-state callback `/3` 返回 `async_r_t`
- **当**runtime 处理返回值
- **那么**runtime 报 invalid sf-cc return
- **并且**不得恢复已消费的 CcRef

### Requirement: Every runner-owned action callback carries one sf_cc_mref

`#callback{}` MUST 包含 `sf_cc_mref = undefined`，且由任意 public runner 创建的每个挂起 action
callback MUST 保存当前 CcRef。
`cc` MUST 继续保存原 ordinary fun 或 stable continuation data，`acc_ref` MUST 保持现有语义。
runtime MUST NOT 用 callback envelope 或 sidecar
mapping 替代该字段。

#### Scenario: Ordinary continuation representation is unchanged

- **给定**ordinary Promise 挂起并保存 anonymous continuation `Fun`
- **当**它在 sf-cc scope 下写入 callback
- **那么**`#callback.cc` 仍精确为 `Fun`
- **并且**新增字段 `sf_cc_mref` 等于顶层 CcRef

#### Scenario: Frame representation is unchanged

- **给定**frame Promise 挂起并保存 stable continuation data
- **当**它在同一 scope 写入 callback
- **那么**`#callback.cc` 仍是 stable data
- **并且**新增字段指向相同 CcRef

### Requirement: sf_cc scope is independent from accumulator scope

runtime MUST 独立传播 `AccRef` 与 optional CcRef，并在 AccRef 重绑定和 continuation resume 时保持 CcRef。
`get_local_ref` MUST 继续返回 AccRef；sf-cc runtime MUST 可读取当前 CcRef。
`local_ref/3` 或内部 parallel/aggregation 重绑定 AccRef 时 MUST 保持 CcRef。
reply executor MUST 从 decoded callback 的 `acc_ref` 与 `sf_cc_mref` 重建两个 scope values，使 resumed
ordinary/frame continuation 创建的下一 ActionMRef 继承同一 CcRef。

#### Scenario: Sequential suspension inherits CcRef

- **给定**ActionA callback 携带 CcRef
- **当**ActionA reply 恢复 continuation，且 ActionB 随后挂起
- **那么**ActionB 的新 callback 携带相同 CcRef
- **并且**ActionA callback 已按 reply lifecycle 删除

#### Scenario: Accumulator ref changes

- **给定**内部 operation 通过 `local_ref/3` 把 AccRef 从 RefA 改为 RefB
- **当**operation 内 Action 挂起
- **那么**callback.acc_ref 为 RefB
- **并且**callback.sf_cc_mref 仍为原 CcRef

### Requirement: Async continuation context is explicit

`async_t` internal action MUST 显式接收 `(K, SfContext)`，且 MUST NOT 通过进程字典传播 continuation 或
runner。ordinary bind MUST 继续构造 anonymous continuation，只透传 `SfContext`；只有 `bind_frame/4`
MAY 创建 `sf_next`。Promise MUST 仅在实际 `K` 与 `SfContext.runner` 相同时保存 structured continuation，
否则保存实际 `K`。

#### Scenario: Ordinary bind remains anonymous

- **给定**stable execution context 内存在 ordinary bind
- **当**该 bind 的 Action 挂起
- **那么**callback.cc 保存 ordinary anonymous continuation
- **并且**ordinary bind 不创建 frame

#### Scenario: Frame bridges an ordinary downstream

- **给定**`bind_frame` 的 downstream 是 ordinary anonymous continuation
- **当**frame Action 挂起
- **那么**runtime 为 anonymous downstream 注册独立 CcRef descriptor
- **并且**callback.cc 保存以该 CcRef 终止的 `sf_next` chain，而不把 fun 嵌入 chain

### Requirement: CcRef entry is the sole final callback owner

每个 runner core MUST 创建唯一顶层 CcRef entry；ordinary/frame representation bridge MAY 创建额外内部
CcRef entry 承接 anonymous downstream：

```erlang
CcRef => #{kind := sf_cc_ref, protocol := 1, cc := FinalCc}
```

`FinalCc` MUST 使用统一 `sf_cc` descriptor encoding。它可以保存显式 sf-prefixed callback、ordinary
library adapter 或 wait completion；只有显式 sf-prefixed API 的 callback 受 external-fun macro contract 约束。
CcRef entry MUST 与 action callbacks 共用现有 callbacks dictionary。entry presence 表示 pending，absence
表示 consumed；不得保留 done tombstone。`callback_exists/1` MUST 只统计 action callback tuple，不能仅因
存在 CcRef entry 返回 true。reply router MUST 把命中 `sf_cc_ref`、accumulator 或其他非-callback value
视为 unhandled。

#### Scenario: Immediate Promise leaves no CcRef

- **给定**Promise 立即产生 Outcome 而不挂起
- **当**通过 sf-cc runner 执行
- **那么**runtime 创建并在同一次执行中消费 CcRef
- **并且**最终 callbacks store 不含 CcRef entry

#### Scenario: CcRef is not a reply target

- **给定**callbacks store 含 `sf_cc_ref` entry
- **当**reply router 收到以 CcRef 为 correlation key 的输入
- **那么**router 返回 unhandled
- **并且**不调用 final callback

### Requirement: Completion consumes and cleans before invocation

所有正常和 terminal failure paths MUST 调用唯一 `complete_sf_cc(CcRef, Outcome, State)` 语义原语。
runtime MUST 在任意用户 final callback 调用前完成：preflight、take CcRef、删除所有
`sf_cc_mref = CcRef` 的 action callbacks，以及清理其 timer、monitor 和 accumulator lifecycle data。
callback `/3` MUST 接收所得 CleanState。CcRef 已缺失时 MUST 返回 unhandled 且不得调用 callback。

#### Scenario: Direct callback sees CleanState

- **给定**State callbacks 中包含 ActionMRef 和 CcRef
- **当**Promise 完成并调用 direct callback `/3`
- **那么**callback 收到的 State 不含 ActionMRef 或 CcRef
- **并且**相关 timer/monitor 已取消

#### Scenario: Multiple internal callbacks share final completion

- **给定**一个 Promise 的内部实现物化多个携带同一 CcRef 的 action callbacks
- **当**该 Promise terminal-complete
- **那么**runtime 清理这些内部 lifecycle entries
- **并且**只调用 final callback 一次
- **并且**该清理不把 CcRef 定义为多个独立业务 Promises 的 result group

### Requirement: Final completion is exactly once and non-rollbackable

runtime MUST 在调用用户 final callback 前消费 CcRef，且任何调用后失败 MUST NOT 恢复或重试该 CcRef。
final callback crash、invalid return、外部副作用、重复 fail 或迟到 reply MUST NOT 恢复 CcRef 或自动 retry。
callback/effect 内部 exception MUST 原样传播。batch fail path MUST NOT 捕获任意业务异常；它只可将
预期的 unavailable final callback、invalid return 或 invalid descriptor 汇总为 `cc_failed`。

#### Scenario: Callback replies then crashes

- **给定**final callback 已向外部进程发送 reply，随后抛出 exception
- **当**迟到 Action reply 或第二次 fail 到达
- **那么**final callback 不再调用
- **并且**CcRef 保持 absent

#### Scenario: Late reply after success

- **给定**Promise 已成功消费 CcRef
- **当**同一 ActionMRef 的迟到 reply 到达
- **那么**router 返回 unhandled
- **并且**不得产生第二次 completion

### Requirement: Message does not consume action or final completion

普通 message path MUST 保留 ActionMRef callback 和 CcRef entry，且 MUST NOT 把 message 当作 final Outcome。
continuation MUST 按现有 message semantics 执行。reply、timeout 和 DOWN 继续按
现有 lifecycle 决定是否消费 ActionMRef；ordinary timeout/DOWN Outcome 默认交给 continuation。

#### Scenario: Message precedes reply

- **给定**protected ActionMRef 先收到 message 后收到 reply
- **当**处理 message
- **那么**ActionMRef 与 CcRef 都保持 pending
- **并且**后续 reply 仍可正常恢复并最终调用 sf_cc 一次

### Requirement: Stable-frame structural failure bypasses unusable continuation

runtime MUST 在 stable-frame continuation 发生结构性不可用时绕过该 continuation，并通过
`#callback.sf_cc_mref` 执行最终 completion。结构性不可用包括 frame protocol/shape 损坏、manifest/MFA
validation failure 或 migration 明确 terminal failure。
runtime structural Outcome MUST 是 `{error, {sf_runtime, Reason}}`。ordinary business error、timeout 和 DOWN
不得仅凭 Outcome shape 自动进入该路径。

#### Scenario: Ordinary anonymous continuation failure propagates

- **给定**ordinary action callback 的 anonymous continuation 因 purge 不可调用
- **当**Action reply 到达
- **那么**runtime 直接调用该 fun，并让 `badfun` 原样传播
- **并且**sf-cc 不生成 terminal Outcome、不调用 CcRef completion

#### Scenario: Corrupt frame does not need terminal traversal

- **给定**frame continuation shape 已损坏但 callback.sf_cc_mref 有效
- **当**runtime 检测 structural failure
- **那么**runtime 直接使用 record field 找到 CcRef
- **并且**不递归解析损坏 frame 来寻找 final callback

### Requirement: Frame terminal stores only CcRef

stable frame continuation terminal MUST 只保存 `#{kind := sf_cc_ref, protocol := 1, mref := CcRef}`，不得嵌入完整 sf_cc descriptor。
terminal mref 与保存该 continuation 的 callback.sf_cc_mref MUST
相等。normal frame completion 到达 terminal 时 MUST 调用统一 completion primitive。

#### Scenario: Nested frame has one final owner

- **给定**nested `sf_next` chain 包含多个 frame
- **当**检查持久化 continuation
- **那么**chain 尾部只包含 CcRef terminal
- **并且**完整 external callback/Context 只存在于 CcRef entry

### Requirement: Callback readers accept only the current encoding

runtime MUST 只把当前 `{callback, Cc, AccRef, SfCcMRef}` record encoding 解码为 action callback。
`{callback, Cc, AccRef}` 等其他 tuple shape MUST NOT 获得兼容执行、CcRef 推断或 fallback Outcome。
所有 callback readers、router、inspection 和 `callback_exists/1` MUST 使用一致 decode semantics。

#### Scenario: Old tuple is not decoded

- **给定**callbacks store 中存在 `{callback, Cc, AccRef}`
- **当**router 或 inspection 读取该 value
- **那么**它不被分类为 action callback
- **并且**runtime 不调用 Cc、不推断 CcRef

### Requirement: Final callback availability handling is exact

external final callback 在 apply 前 MUST 使用 `code:ensure_loaded/1` 与
`erlang:function_exported/3` 检查可用性。`nofile` 或目标 MFA 不再导出 MUST 被分类为
`{sf_runtime, unavailable_continuation}`；其他 module load error MUST 原样报错。实际 apply MAY 只捕获
`error:{badfun, Callback}`，其中 term MUST 与正在调用的 Callback 完全相同。callback 内部或 downstream
产生的 `badfun`、`undef` 和其他 exception MUST 原样传播。

#### Scenario: Nested callback error is not reclassified

- **给定**final callback 已成功进入，但内部调用另一个失效 fun 或缺失 MFA
- **当**内部调用抛出 `badfun` 或 `undef`
- **那么**异常原样传播
- **并且**runtime 不把它改写为 unavailable final callback

### Requirement: Matcher inspection exposes safe callback metadata

`inspect_sf_callbacks/3` MUST 对每个 action callback 同步调用一次 `Matcher/1`，并且只向 matcher 暴露 sanitized summary。
summary MUST 包含 request MRef、optional sf_cc_mref、protected、continuation kind 和 current-to-downstream frame summaries。
每个 frame summary MUST 公开 Module、Vsn、protocol、BlockPath、StepName 和 AsyncId。Vsn MUST 是 frame
producer 保存的 module `-vsn` literal term或 `undefined`，不得限制为 atom。ordinary fun 的 frames MUST
为空；不得从 closure 推断 source metadata。summary MUST NOT 包含 Env、State、timer、monitor、accumulator、
continuation value、final callback 或 Context。

#### Scenario: User matches a nested frame

- **给定**一个 callback 的 frames 包含不同 owner modules
- **当**Matcher 使用 `lists:any` 按 Module/Vsn/BlockPath/StepName 筛选
- **那么**它可选中该 callback
- **并且**runtime 不把非目标 frame metadata 丢失

#### Scenario: Matcher sees no private data

- **给定**frame Env 和 final CC Context 含敏感 term
- **当**Matcher 接收 summary
- **那么**summary 不含这些 term

### Requirement: Matcher failures are preflight-atomic

inspection/failure MUST 在 mutation 前按 RequestMRef term order 完成 matcher preflight，并在 matcher 失败时保持 State 不变。
Matcher exception MUST 原样传播；non-boolean return MUST 抛出 `{invalid_matcher_return, Value}`。
`fail_sf_callbacks/4` MUST 在任意 State mutation 前运行完所有 matcher；
任一 matcher 失败时 State bit-for-bit 不变，且不调用 final callback。

#### Scenario: Matcher crashes after an earlier match

- **给定**Matcher 对第一个 callback 返回 true，对第二个 callback crash
- **当**调用 `fail_sf_callbacks/4`
- **那么**matcher exception 原样传播且 State 保持原值
- **并且**第一个 callback/CcRef 仍存在且 final callback 未调用

### Requirement: Active failure deduplicates by CcRef

`fail_sf_callbacks/4` MUST 以 `{error, {sf_aborted, Reason}}` 完成选中的受保护 callbacks，并按 CcRef 去重。
preflight 后 MUST 以 CcRef term order commit；一个 CcRef 只调用一次。selected unprotected callback MUST 返回
`unprotected` 且不删除。protected per-CcRef result status MUST 是 `completed | cc_failed | unhandled`。
`cc_failed` MUST 不携带 Class、Reason 或 Stacktrace。invalid descriptor 在消费前产生该 status；预期的
unavailable callback 或 invalid return 在消费后产生该 status。callback/effect 内部 exception MUST 原样
传播并停止 commit；已经消费的 CcRef 不回滚。

#### Scenario: Two selected callbacks share CcRef

- **给定**Matcher 选中两个携带同一 CcRef 的内部 callbacks
- **当**主动失败
- **那么**runtime 调用 final callback 一次
- **并且**结果按一个 CcRef 报告

#### Scenario: Selected current callback without CcRef is unprotected

- **给定**Matcher 选中一个当前四元 callback，且 `sf_cc_mref = undefined`
- **当**主动失败
- **那么**result status 是 unprotected
- **并且**callback 保持不变

### Requirement: Module version metadata uses literal module vsn terms

frame producer MAY 保存显式 module `-vsn(Vsn)` literal term，且 sf-cc matcher MUST 原样暴露该 term，不得强制转换类型。
matcher 不得把该 term 转成 atom/string，也不得把 `module_info(attributes)` 或 BEAM attribute 的外层 list 当作 Vsn。
module 未提供可用
explicit Vsn 时 MUST 使用 `undefined`；自动 checksum normalization 属于 migration/versioning follow-up。

#### Scenario: Tuple version term is preserved

- **给定**module 声明 `-vsn({2, 4, 0})`
- **当**frame summary 暴露 Vsn
- **那么**值精确为 `{2, 4, 0}`
- **并且**不是 atom 或 `[{2,4,0}]`
