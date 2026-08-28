# Why sf-do Exists

## 版本升级中的现实困难

普通 `do` 使用 local anonymous fun 表示 bind continuation。异步 action 挂起后，这个 fun 可能被
长期保存在 process State、ETS 或 callback store 中，并跨越 module hot-code upgrade。

local fun 绑定创建它的 code version，而不是绑定一段抽象上“没有变化”的业务逻辑。module 加载
新版本后，只要旧 code 尚未 purge，旧 fun 仍可能正常执行旧实现；旧 code purge 后，fun term 仍会
留在 State 或 ETS 中，但再次调用会失败。现代 OTP 不会因为 State 中持有这种 fun 就在 purge 时
立即终止进程，因此问题通常不会在升级阶段暴露，而会延迟到 reply、timeout 或 `DOWN` 到达、
continuation 真正恢复时出现。

这使普通 continuation 难以安全地跨版本持久化：

- 是否兼容取决于 fun 所引用的 code version 是否仍存在，不能以 fun body 看起来是否变化判断；
- module 中其他代码的变化也可能产生新的 code image，不能把“该段源码未改”当作安全边界；
- State 中仍能看到 fun term，不代表它仍可调用；
- 升级完成与故障出现之间可能相隔很久，故障难以定位，也无法在发布当下可靠发现。

## `sf_do` 的设计回应

`sf_do` 不改变普通 `do` 的 Monad sequencing 语义，也不引入 workflow VM。它只改变可能跨
suspension 保存的 continuation 表示：

```text
ordinary do: Action + local anonymous continuation fun
sf_do:       Action + external MFA + explicit Env data frame
```

挂起时，stable callback 保存 frame 数据，而不是 transform 生成的 remainder closure。恢复时，
runtime 通过 external MFA 进入当前加载版本的 generated function；该 function 从 Env 恢复所需
bindings、消费 action outcome，并返回后续 Monad。

source 可以在非最终 action 前用 standalone literal atom qualifier 显式提供 StepName。runtime 只用
lexical BlockPath 和 StepName 判断当前版本是否仍声明同一个恢复点；它不比较 Action AST，也不从
source index 或代码相似度派生恢复身份。缺少 StepName 的 boundary 仍可普通执行，但 transform
必须产生 warning，且该 frame 不获得 migration 资格。

因此，`sf_do` 解决的是由 transform 生成的 bind/then continuation 对旧 code version 的隐式依赖。
frame identity 与 Env compatibility 仍须按 `sf-do` 和 `sf-migrate` 合同判断；`sf_do` 不根据源码
相似度猜测版本兼容性，也不自动迁移历史 callback。

## Env 中的 fun

`sf_do` 只消除自己生成的 anonymous continuation，不扩展权限去递归检查或改写 Action、Env、
State 或用户 context 中的任意值。

Env 原则上不得包含 anonymous/local fun。若应用确实需要保存 fun-valued capture，应将其视为显式
迁移内容：由 `sf-migrate` 的 directional Env migrator 使用目标版本代码重新生成 fun，或转换为
external MFA/纯数据 descriptor。原因是 State 中的旧 fun 不会在升级时自动清除，却可能在旧 code
purge 后失效。

该迁移职责目前是设计说明，不是 `sf_do` 的强制检查或当前实现验收条件；后续可以在
`sf-migrate` 中补充 detection、migration policy 或 `unstable_capture` 处理。

## 边界

`sf_do` 不负责阻止 code purge、清理 process State、让 ordinary local fun 自动跨版本可用，或把
ordinary fun callback 自动转换成 stable frame。它建立的边界仅是：由 transform 生成且可能跨
suspension 保存的 continuation 使用 external data frame；业务主动保存的 local fun 由应用和显式
migration 负责。

该边界通过显式 `SfContext` 参数传播，不使用进程字典。ordinary bind 仍产生 anonymous continuation，
只有 `bind_frame` 创建 frame。当 stable frame 的 downstream 是 ordinary fun 时，runtime 把该 fun
物化为同一 callbacks store 中的 continuation node；stable callback 只保存指向该 node 的 `next_ref`。

## 最终完成不属于 frame 本身

frame 只描述“收到 Input 后从哪个稳定边界继续执行”，不拥有完整父层或最终 callback。父层同样是
callbacks store 中的一个 node；当前 stable callback 只保存直接 `next_ref`。最外层 callback 只是
`next_ref = none` 的叶节点，不使用另一种 FinalCcRef 类型。

这种表示让 normal completion 和 failure 都遵循同一条逐节点路径。success 执行当前 frame；frame
resolution、migration 或 invocation failure 跳过普通 success frame并交给直接外层。若外层仍不能处理，
继续沿 `next_ref` 传播；显式 failure handler 可以在任意层消费。系统不需要在每层复制 root reference。

`wait`、ordinary anonymous callback runner 和 explicit sf-cc runner 可以把自己的 completion 作为同一种
continuation node 注册；只有显式 external fun 获得 compile-time source guarantee。调用已经成功进入
用户 callback 后产生的业务 exception 不得被误分类为 continuation availability failure。

## 外部 owner module 的升级

持有 callbacks State 的进程与定义 generated frame 的 module 可以不同。owner module 独立升级不会
自动触发 holder process 的 `code_change/3`，所以 eager migration 只能是优化，不能是 correctness
前提。每个 persisted frame 必须自带 owner Module、source Vsn、identity 和 Env，并在实际调用 generated
MFA 前逐节点 resolve。nested chain 中来自其他 modules 的 frame 保持原样，直到轮到该节点执行。

sf-do 负责产生这类 self-describing frame 和 current target manifest；是否映射 endpoint、转换 Env/Input、
使用 optional historical metadata 或在 downgrade 时 fail closed，属于 sf-migrate。无法 resolve 时不猜测
旧调用栈或重放前置 action，而是把 failure 交给当前 node 的直接 `next_ref`。
