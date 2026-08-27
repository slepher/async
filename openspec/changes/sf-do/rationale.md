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
只有 `bind_frame` 创建 frame。当 stable frame 的 downstream 是 ordinary fun 时，runtime 以独立 CcRef
descriptor 保存该 fun，使 structured chain 仍以纯数据 CcRef terminal 结束。

## 最终完成不属于 frame 本身

frame 只描述“收到 Input 后从哪个稳定边界继续执行”，不应同时拥有 promise 的最终 callback。
完整 final callback 若嵌在 continuation chain 尾部，一旦 frame shape 已损坏，
runtime 仍要解析恰好已经失效的数据才能找到 fallback；这与 fallback 存在的目的冲突。

因此受 `sf-cc` 保护的 action callback 分别保存两条正交关系：`cc` 保存 ordinary fun 或 stable frame
continuation，`sf_cc_mref` 保存 promise 的一次性最终完成地址。sf-do chain 只保存指向同一 CcRef 的
`sf_cc_ref` terminal；完整 callback/Context 由 sf-cc 的 CcRef entry 独占。这样 frame 正常完成、
frame validation failure、migration failure 和人工终止可以收敛到同一个 cleanup-before-invoke
completion primitive，而不要求 sf-do 理解 callback `/2`、`/3` 或 State return contract。

CcRef adoption 不以 API 名称为边界。`wait`、ordinary
anonymous callback runner 和 explicit sf-cc runner 都可以无感使用统一 CcRef lifecycle；它们的区别
只在 final callback 的稳定性证明：只有显式 external fun 获得 compile-time source guarantee。

ordinary action continuation 仍由普通 bind executor 直接调用。它因 code purge 产生的 `badfun` 以及
callback 内部的 `undef`/其他 exception 原样传播；sf-do 和 sf-cc 不负责额外 unavailable-code cleanup、
分类或合成 fallback Outcome，普通 reply lifecycle 的既有处理不变。

## 外部 owner module 的升级

持有 callbacks State 的进程与定义 generated frame 的 module 可以不同。owner module 独立升级不会
自动触发 holder process 的 `code_change/3`，所以 eager migration 只能是优化，不能是 correctness
前提。每个 persisted frame 必须自带 owner Module、source Vsn、identity 和 Env，并在实际调用 generated
MFA 前逐节点 resolve。nested chain 中来自其他 modules 的 frame 保持原样，直到轮到该节点执行。

sf-do 负责产生这类 self-describing frame 和 current target manifest；是否映射 endpoint、转换 Env/Input、
使用 optional historical metadata 或在 downgrade 时 fail closed，属于 sf-migrate。无法 resolve 时不猜测
旧调用栈或重放前置 action，而是通过 `sf_cc_mref` 终止整个 promise。
