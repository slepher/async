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
legacy fun callback 自动转换成 stable frame。它建立的边界仅是：由 transform 生成且可能跨
suspension 保存的 continuation 使用 external data frame；业务主动保存的 local fun 由应用和显式
migration 负责。
