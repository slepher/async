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

- 新增 `sf_do/1`、`sf_do/2` 和可选 named-step `sf_do/3` source syntax；表达式结果仍是声明
  Monad 的普通 monadic value，不生成独立 `sf_program()` 类型。
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
- stable async 新增专有数据 `sf_cc` completion interface。用户显式传递 external MFA/context；
  stable path MUST NOT 调用 legacy `CC` fun、`callback_to_cc` 或原 `run_cc` interface。
  `sf_cc` 不属于 `sf_do` block identity，也不参与 migration。
- timeout、DOWN 和普通 reply 都作为 action outcome 传给当前 frame。pattern/后续 Monad
  决定如何处理；它们不是 stable runtime 的特殊 terminal failure。
- `sf_do` 仍是普通 Monad，因此 nested `sf_do` 可直接作为外层 `bind_frame` 的 Action。
  nested block 使用 lexical block path 防止名称冲突，但无需 `sf_subprogram` 或 frame VM。
- Env 只保存当前 continuation 实际需要的 lexical bindings，不预测未来版本需求。未来目标
  continuation 缺少数据且 `sf-migrate` Env migrator 没有先行补齐时，按 code-change direction
  确定性失败；变量改名或同名变量含义变化的显式 Env 转换属于 `sf-migrate`，`sf_do` 本身不定义
  migration DSL 或重建策略。
- Env 原则上不应包含 anonymous/local fun。`sf_do` 不递归检查 capture runtime values；若业务仍
  捕获 local fun，升级不会自动清除或替换该 term，旧 code purge 后它可能不可调用。应用应通过
  `sf-migrate` Env migrator 用目标版本代码重新生成 fun，或改存 external MFA/data descriptor；
  自动检测与强制迁移不属于当前 `sf_do` 验收。
- derived identity 使用 protocol、logical block、非最终 continuation-boundary index 和 canonical Action；可选 named identity
  使用 BlockName + StepName。后续 continuation、captures 和 runtime Env 不参与 AsyncId。
- callbacks 继续使用现有 callbacks field、request correlation key、monitor、timer、accumulator
  reference 和 `async_r_t` State/store 机制。

## Source Forms

```erlang
sf_do([Monad || Qualifiers])
sf_do(BlockName, [Monad || Qualifiers])
sf_do(BlockName, StepNames, [Monad || Qualifiers])
```

最后一个参数必须是 OTP 21 可解析的 single-head list comprehension。BlockName/StepNames 不得
放入 comprehension head。StepNames 只覆盖具有 block-local remainder 的非最终 monadic
qualifiers；pure let 和 final action 不消耗名称。

## Compatibility Boundary

- ordinary `do`、legacy callbacks 和 ordinary bind 保持原行为。
- stable frame path 显式启用；不得把 legacy closure callback 自动宣称为可迁移 frame。
- frame identity 相同只表示等待同一个 async action，不保证 source Env 满足未来 continuation。
- runtime 不按 AST、index 接近度、capture 名称或值猜测 migration target，也不重放 boundary
  之前的 action。

## Capabilities

### New Capabilities

- `sf-do`: frame-aware do transform、per-boundary stable functions、lexical Env、专有 data
  `sf_cc` completion interface、
  nested Monad composition 和 current manifest。

## Impact

- `async_sf_transform` 改为 Astranaut-based analyzer/rewrite pipeline。
- `monad_frame` 明确为保留 Monad 类型的 `bind_frame`/`then_frame` sequencing capability。
- `async_t` 增加 frame-aware bind/run 路径，在 suspended callback 中保存 data continuation。
- callback entry 增加 generated next frame 与 downstream stable continuation/`sf_cc` data，但不增加第二个 callbacks
  store。
- `async_m` 暴露使用显式 `sf_cc` MFA descriptor 的专有 stable runner facade；不复用 legacy
  CC runner。
