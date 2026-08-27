# sf-do

## Why

普通 ErLando `do` 把每个 bind continuation 编译成 anonymous fun。async action 挂起时，
该 fun 会进入 callback store；旧代码 purge 后，它不能作为跨版本 continuation 恢复。

`sf_do` 的目标不是引入一种新的 program/VM，而是保留原 Monad 类型，只把普通 bind 的
anonymous continuation 换成数据 frame：

```text
bind       : M A -> (A -> M B) -> M B
bind_frame : M A -> Frame(A -> M B) -> M B
then_frame : M A -> Frame(_ -> M B) -> M B
```

frame 携带 stable identity、external MFA 和显式 Env。挂起时 callback 保存 frame；reply
到达后调用当前版本 MFA，得到原类型的 Monad 并继续运行。

该设计所针对的版本升级困难和延迟失效边界见 [Why sf-do Exists](rationale.md)。

## What Changes

- 新增 `sf_do/1`、`sf_do/2` source syntax；表达式结果仍是声明 Monad 的普通 monadic value，
  不生成独立 `sf_program()` 类型。StepName 作为 standalone literal atom qualifier 内联在其修饰的
  非最终 action 前，不再使用额外 list/record 参数。
- 新增 `monad_frame` capability。兼容 Monad 的 `bind_frame`、`then_frame` 必须保持 Monad
  类型，只把 continuation 从 fun 改为 frame；`default_then_frame` 委托 `bind_frame`。
- parse transform 仿照 ErLando `do` 使用 Astranaut AST traversal：有绑定的非最终
  `Pattern <- Action` 统一编译成 `monad_frame:bind_frame(Action, NextMFA, FrameContext, Monad)`；
  无绑定的非最终 `Action` 统一编译成
  `monad_frame:then_frame(Action, NextMFA, FrameContext, Monad)`。transform 不按 Monad AST
  的 atom/variable/tuple 形状直调实例 module；运行时 typeclass dispatcher 负责解析。`then_frame` 的 typeclass
  default 直接调用 `bind_frame`，对应 `classic_do` 用忽略 Reply 的 bind continuation 实现
  sequencing。每个这类 continuation boundary 生成 external function。
- generated frame function 固定接收 reply/Input 与 Env；`bind_frame` frame 用 Input 匹配 pattern，
  `then_frame` frame 忽略 Input。两者恢复 captures 后都直接返回后续 Monad；
  不返回 `{await, ...}`、`{done, ...}` 等解释器指令。
- 非变量 `<-` pattern 使用与 `classic_do` 相同的 fallback：匹配失败返回 Monad
  `fail(monad_badmatch)`。纯 `Pattern = Expression` 保持普通 Erlang match 语义。
- 保留 `classic_do` 对 block 内 local `return/1`、`fail/1` 的 scoped rewrite；nested block 使用
  最内层声明的 Monad。最后一个 qualifier 必须是 `return(A)` 或其他 Monad action，直接作为
  remainder 返回，不生成 terminal frame。
- stable async 的最终 completion 由独立 `sf-cc` capability 提供。用户通过 sf-cc runner 显式传递
  external fun/Context；`sf_do` 不定义 callback arity/return contract，也不生成 CC MFA。
  frame chain 只以 `sf_cc_ref` CcRef terminal 结束；完整 `sf_cc` 不属于 `sf_do` block identity、
  manifest 或 migration。
- timeout、DOWN 和普通 reply 都作为 action outcome 传给当前 frame。pattern/后续 Monad
  决定如何处理；它们不是 stable runtime 的特殊 terminal failure。
- `sf_do` 仍是普通 Monad，因此 nested `sf_do` 可直接作为外层 `bind_frame` 的 Action。
  nested block 使用 lexical block path 防止名称冲突，但无需 `sf_subprogram` 或 frame VM。
- Env 只保存当前 continuation 实际需要的 lexical bindings，不预测未来版本需求。未来目标
  continuation 缺少数据且 `sf-migrate` Env migrator 没有先行补齐时，按 code-change direction
  fail closed 并通过当前 CcRef terminal-complete；具体 Outcome 由 `sf-migrate` 定义。变量改名或同名
  变量含义变化的显式 Env 转换属于 `sf-migrate`，`sf_do` 本身不定义 migration DSL 或重建策略。
- Env 原则上不应包含 anonymous/local fun。`sf_do` 不递归检查 capture runtime values；若业务仍
  捕获 local fun，升级不会自动清除或替换该 term，旧 code purge 后它可能不可调用。应用应通过
  `sf-migrate` Env migrator 用目标版本代码重新生成 fun，或改存 external MFA/data descriptor；
  自动检测与强制迁移不属于当前 `sf_do` 验收。
- 有 StepName 的 continuation boundary identity 只使用 protocol、logical BlockPath 和 StepName；
  AsyncIndex、Action AST、后续 continuation、captures 和 runtime Env 均不参与 AsyncId。AsyncId
  只是该 named tuple 的固定长度编码，不是额外的 source-similarity 判断。缺少 name 的 boundary
  允许普通执行但产生 compiler warning，并且在 migration 时确定性失败。
- transform 第一遍必须用 Astranaut 的普通错误结果累计 source diagnostics；第一遍存在任何错误时
  直接通过 `astranaut_return:to_compiler/1` 返回 compiler errors，不运行第二遍。第二遍只消费冻结
  plan，不得重新执行 source validation、identity 或 capture analysis。
- callbacks 继续使用现有 callbacks field、request correlation key、monitor、timer、accumulator
  reference 和 `async_r_t` State/store 机制。受 sf-cc 保护的新 action callback 在尾字段
  `sf_cc_mref` 保存 CcRef；`cc` 仍保存原 frame continuation。
- 所有 public runners 新启动的 Promise 都进入同一 CcRef infrastructure。`wait/1`、`wait_t/2`
  保持原调用和正常结果；ordinary callback runner 继续接受原 callback source/return contract，并通过
  ordinary `sf_cc` completion descriptor 接入同一 lifecycle。
- `async_t` 通过显式 `(K, SfContext)` 传播 structured continuation context，不使用进程字典。ordinary bind
  继续创建 anonymous continuation；只有 `bind_frame` 创建 `sf_next`。当 frame downstream 是 ordinary
  fun 时，runtime 以独立 CcRef descriptor 桥接，不把 fun 嵌入 frame chain。
- callback reader 只接受当前四元 record encoding，不提供旧三元 tuple 或 tagged outcome compatibility。
- 每个 persisted frame 保存 owner Module 和 module `-vsn` literal term或 `undefined`。Vsn 不进入
  AsyncId；它只用于 inspection 和 migration endpoint selection。
- 每个 frame 执行前都有 owner-module resolution 边界，使外部 owner module 独立升级时可以在
  reply-time lazy resolve。resolution/migration 失败时，runtime 通过 callback 的 `sf_cc_mref`
  terminal-complete，不递归遍历 continuation 寻找 final callback。

## Source Forms

```erlang
sf_do([Monad || Qualifiers])
sf_do(BlockName, [Monad || Qualifiers])
```

最后一个参数必须是 OTP 21 可解析的 single-head list comprehension。BlockName 不得放入
comprehension head。standalone literal atom qualifier 只能作为紧随其后的非最终 `bind`/`then`
action 的 StepName marker；marker 不执行。literal atom qualifier 出现在其他位置必须报 error。
未被 marker 修饰的非最终 action 产生 warning；pure let 和 final action 不需要 name。

transform diagnostic 使用严格的领域 `format_error/1` clauses；compiler 负责 file、line、column、原代码
摘录和位置标记，领域消息只说明规则与允许形式。正常 compiler 路径的未知 reason 由 Astranaut adapter
兜底，测试则直接调用领域 formatter，使 reason 拼写错误或遗漏 clause 以 `function_clause` 暴露。同一
duplicate StepName/BlockPath 或 trailing marker 根因不得再产生派生 collision/invalid-last 噪音。

## Compatibility Boundary

- ordinary `do`、ordinary callbacks 和 ordinary bind 保持原行为。
- stable frame path 显式启用；不得把 ordinary closure callback 自动宣称为可迁移 frame。
- callback record 由 `sf-cc` 统一定义为当前四元 encoding；不提供旧三元 tuple compatibility。
- ordinary runner 也创建 CcRef，但 ordinary action continuation 的 `badfun` 或内部异常原样传播；
  sf-do/sf-cc 不把它转换为 structural terminal Outcome。
- named frame identity 相同只表示相同 BlockPath/StepName，不保证 source Env 满足未来 continuation。
- runtime 不按 AST、index、action 内容、capture 名称或值猜测 migration target，也不重放 boundary
  之前的 action。没有 StepName 的 frame 在普通执行中可用，但在 migration 中默认不兼容。
- upgrade 不以 historical old block 为运行必要条件；sf-do 只提供实际 stored source frame 和 current
  target manifest。Env/Input conversion、migration catalog 和 strict downgrade metadata policy 属于
  `sf-migrate`。

## Capabilities

### New Capabilities

- `sf-do`: frame-aware do transform、per-boundary stable functions、lexical Env、CcRef terminal、
  owner/version metadata、逐 frame resolution boundary、nested Monad composition 和 current manifest。

### Dependencies

- `sf-cc` owns the CcRef engine, stable final runners, ordinary runner adapters, current callback encoding,
  exactly-once completion and active inspection/failure.
- `sf-migrate` owns endpoint mappings, optional Env/Input conversion, historical metadata and downgrade
  policy. `sf-do` supplies the self-describing frames and target manifest consumed by that capability.

## Impact

- `async_sf_transform` 改为 Astranaut-based analyzer/rewrite pipeline。
- `monad_frame` 明确为保留 Monad 类型的 `bind_frame`/`then_frame` sequencing capability。
- `async_t` 增加 frame-aware bind/run 路径，在 suspended callback 的 `cc` 中保存 data continuation，
  并继承 sf-cc scope 提供的 `sf_cc_mref`。
- callback entry 增加 generated next frame 与 downstream `sf_cc_ref` data；CcRef owner entry 与 action
  callback 共用同一个 callbacks dictionary，不增加第二个 callbacks store。
- `async_sf_runtime` 在调用 stored frame MFA 前进入 owner-module resolver，并把 normal terminal 或
  structural failure 交给 `sf-cc` completion primitive。
- `async_m` 的 public final runner facade 由 `sf-cc` 定义；`run_sf_frame/4` 不再是 sf-do 对外冻结的
  final completion API。
- existing `wait`/ordinary runner facades 保持 source/result contract，并使用同一 CcRef engine；ordinary
  continuation invocation errors 保持原异常语义。
