# Haskell 中 Monad Transformer 与逻辑组合的现代方案

## 问题范围

本文只讨论 Haskell，不讨论当前 Erlang `async` 项目的实现方案。

问题不仅是如何管理 IO 等副作用，而是：

> Monad Transformer 曾被用来叠加和组合计算逻辑。经过多年发展，Haskell
> 中是否已经有更现代的替代方案？

结论是：

> 没有一个可以普遍取代 Monad Transformer 的“Monad 组合器”。现代方法是先
> 区分独立计算、动态依赖、模块能力、DSL 语法和真正的计算语义叠加，
> 再选择对应的最小抽象。

## Monad 为什么不能任意组合

给定两个 Monad：

```haskell
m :: Type -> Type
n :: Type -> Type
```

一般不能自动得到 `Compose m n` 的合法 `Monad` 实例。两层计算如何交换
需要额外语义，例如：

- 失败影响单个分支还是整个搜索；
- 状态在分支之间共享还是复制；
- 失败后状态保留还是回滚；
- 外层结构能否与内层结构交换。

Monad Transformer 的价值就在于为这些交互提供一种具体语义，而不只是把两个
类型包在一起。

## 方案一：继续使用 `mtl`/`transformers`

`mtl` 仍是 Haskell 生态的基础设施，而不是已被废弃的历史方案。现代用法通常不在
业务函数中暴露具体 stack：

```haskell
process
  :: (MonadReader Config m, MonadError AppError m, MonadIO m)
  => Input
  -> m Output
```

只在程序边界选择：

```haskell
newtype AppM a = AppM
  { runAppM :: ReaderT Config (ExceptT AppError IO) a }
```

优点：

- 生态和第三方库兼容性最好；
- 类型推导成熟；
- 小型 stack 简单直接；
- 某些专用 transformer 仍是 parser、stream、search 和 continuation 的自然表示。

局限：

- transformer 顺序会改变语义；
- lifting 和实例组合可能复杂；
- 同时使用多个同类型 Reader/State 不方便；
- 异步异常、资源和 StateT 的交互可能微妙。

参考：<https://hackage.haskell.org/package/mtl>

## 方案二：`ReaderT AppEnv IO` 与 Handle Pattern

对普通应用，可以把环境和能力放入 record：

```haskell
data AppEnv = AppEnv
  { logger   :: Logger
  , database :: Database
  , clock    :: Clock
  }

newtype App a = App
  { runApp :: ReaderT AppEnv IO a }
```

或者直接将模块表示为 functions record：

```haskell
data UserStore m = UserStore
  { findUser :: UserId -> m (Maybe User)
  , saveUser :: User -> m ()
  }
```

优点是简单、易懂、IO 互操好，并且很适合业务模块组合。局限是效果集合通常不会
在类型中精确列出，可重解释能力也弱于 extensible effect system。

## 方案三：可扩展 Effect System

### `effectful`

`effectful` 的 `Eff` 可以理解为可扩展的、强化版 `ReaderT IO`。它支持静态和
动态 dispatch，并重视异常语义、性能和现有 IO 生态互操。

```haskell
process
  :: (Reader Config :> es, Error AppError :> es, IOE :> es)
  => Input
  -> Eff es Output
```

适合普通 Web、CLI、数据库和并发 IO 应用。它不支持需要任意捕获并多次恢复剩余
计算的 handler，因此 nondeterminism 和 coroutine 等场景仍需要专用抽象。

参考：<https://hackage.haskell.org/package/effectful>

### `fused-effects`

`fused-effects` 将 effect 语法与 carrier/interpreter 分离，支持 scoped/higher-order
effects，并通过融合避免 freer 中间结构。

优点是表达力和语义严谨，性能目标接近 `mtl`。代价是自定义 effect/carrier
的学习成本和样板高于 `effectful` 或 `polysemy`。

参考：<https://hackage.haskell.org/package/fused-effects>

### `polysemy`

`polysemy` 使用 freer/higher-order effect 风格，定义 effect 和 interpreter 的样板少，
很适合可重解释 DSL。其官方文档已说明，早期的“零成本”优化在大型、多模块
程序中不稳定，因此它不应被当作默认的性能首选。

参考：<https://hackage.haskell.org/package/polysemy>

### `Bluefin`

Bluefin 使用值级 capability/handle，不主要依赖隐式 typeclass 搜索 effect：

```haskell
increment :: State Int s -> Eff es ()
increment stateHandle =
  modify stateHandle (+1)
```

不同 effect 实例由不同 handle 区分，作用域明确，同时存在多个同类型 State/Reader
也较自然。它代表了值级 capability 的新方向，但生态和 API 成熟度仍低于
`mtl` 和 `effectful`。

参考：<https://github.com/tomjaguarpaw/bluefin>

## 方案四：组合操作语言，而不是组合 Monad

### Tagless-final / Capability Typeclasses

业务逻辑只声明它需要的能力：

```haskell
class Monad m => UserRepo m where
  findUser :: UserId -> m (Maybe User)

class Monad m => Mailer m where
  sendMail :: Email -> Message -> m ()

notifyUser
  :: (UserRepo m, Mailer m)
  => UserId
  -> Message
  -> m ()
```

组合发生在操作接口层，具体 Monad stack 只在边界选择。这对业务模块化通常
比为每个模块定义 `FooT` 更合适。

### Initial Encoding / Free Structure

如果程序不仅需要执行，还需要分析、优化、记录或编译，可以将逻辑表示成
AST，并通过 sum/coproduct 组合多种操作语言。

可根据结构选择：

- Free Applicative：操作独立，结构静态；
- Selective DSL：有限条件分支；
- Free Monad：后续程序依赖之前的运行结果；
- Final tagless：不保存 AST，在构造时选择解释。

## 方案五：不默认使用 Monad

许多过去写成 monadic pipeline 的逻辑可以用更弱、更准确的抽象。

### 普通函数

```haskell
process :: PriceTable -> RawOrder -> OrderSummary
process table =
  summarize . price table . normalize
```

纯数据变换应首先使用函数组合和明确的中间类型。

### `Applicative`

当多个计算相互独立，但需要合并结果时：

```haskell
mkUser
  <$> validateName rawName
  <*> validateAge rawAge
  <*> validateEmail rawEmail
```

Applicative 保留静态结构，因此更容易进行并行、批处理、分析和错误累积。

### `Selective`

Selective 位于 Applicative 与 Monad 之间：允许条件执行，但仍尽可能保留静态
结构。它适合配置验证、构建系统和可预先分析的工作流。

抽象能力大致为：

```text
Functor < Applicative < Selective < Monad
```

越靠右表达力越强，但静态分析和自动优化的空间通常越少。

### 显式 ADT 和 Reducer

将业务决策表示为纯状态转换：

```haskell
decide
  :: OrderState
  -> OrderEvent
  -> Either DecisionError (OrderState, [Command])
```

外部交互由命令解释器执行。这比将状态、错误、Writer 和 IO 全部隐藏在一个
transformer stack 中更容易理解。

### 显式状态机

对订单、审批、协议和游戏逻辑，可用 ADT/GADT 表达合法状态和转换，而不是用
`StateT` 隐藏领域状态。

### Streaming、Dataflow 和 FRP

如果核心逻辑是持续数据流、依赖图或随时间变化的事件，应使用对应的专用抽象，
而不是将所有逻辑压进一个 `m a`：

- `conduit`/`pipes`/`streaming`：持续流处理；
- DAG/dataflow：依赖调度和自动并行；
- FRP：事件、时间和状态变化。

## GHC 原生能力

GHC 9.6 引入了 `prompt#` 和 `control0#` 等 delimited-continuation primops，供库作者
构建 effect handler。`base` 中没有为它们提供安全、标准的高层 effect API。

因此 Haskell 当前仍是在选择库级 effect 方案，而不是启用一个统一的语言级
effect system。

参考：<https://downloads.haskell.org/ghc/9.6.1/docs/users_guide/9.6.1-notes.html>

## 选型建议

| 问题 | 优先方案 |
| --- | --- |
| 纯数据转换 | 普通函数和 ADT |
| 多个独立计算 | Applicative |
| 有限条件依赖 | Selective |
| 后一步动态依赖前一步的值 | Monad/Kleisli |
| 错误累积 | Validation Applicative |
| 业务模块组合 | Handle/record 或 tagless-final capability |
| 需要分析、编译的 DSL | Free Applicative/Selective/Free Monad |
| 小型、成熟、重生态兼容 | `mtl`/`transformers` |
| 普通 IO 应用的可扩展 effects | `effectful` |
| 高阶/scoped effects 与可重解释 DSL | `fused-effects` |
| 低样板的可重解释 DSL | `polysemy` |
| 值级 capability 和明确作用域 | Bluefin |
| 持续流、搜索、parser、continuation | 专用 transformer/专用抽象 |

## 实用优先级

编写新逻辑时，可以按以下顺序考虑：

1. 普通纯函数和明确的输入/输出类型；
2. product/sum ADT 表达组合与选择；
3. 独立计算使用 Applicative；
4. 有限条件依赖使用 Selective；
5. 模块依赖使用 handles/records；
6. 状态变化使用 reducer/显式状态机；
7. 需要检查程序时构造 AST/DSL；
8. 真正存在运行时数据依赖时使用 Monad；
9. 真正需要叠加计算语义时使用 Monad Transformer 或 effect handler。

## 总结

Monad Transformer 没有被一种新抽象全面淘汰。现代 Haskell 的进步主要是：

- 不再用 transformer tower 承担所有模块化职责；
- 优先使用函数、ADT、Applicative 和 Selective；
- 用 handles/tagless-final 组合业务能力；
- 用 AST/Free 结构组合可解释语言；
- 需要精确 effect row 和解释器时选择现代 effect system；
- 只有真正的动态数据依赖才需要 Monad；
- 只有真正的计算语义叠加才需要 Monad Transformer。
