# 审核原则

## Continuation runtime 边界

1. 正常 dispatch 是生命周期代码，不是 store 审计代码。主路径保持
   `lookup -> take -> cleanup -> invoke/forward`；不得在每次 message、reply 或 failure
   中递归检查 ancestors、descendants、reverse index、cycle、depth、timer metadata 或
   accumulator metadata。
2. 只在最早的责任边界检查必要条件。注册可以一次性检查 key shape、node/action kind、
   duplicate key 和 direct parent，然后提交 entry 与 reverse ownership edge。
3. Backend 完整性、持久化 State 兼容和热升级转换属于 backend/upgrade 边界，不得混入正常
   runtime 路径。若该边界需要完整一致性检查，应在那里显式执行一次，而不是在 dispatch
   期间反复进行局部检查。
4. Entry 缺失必须按 owner 分类：external ActionMRef 缺失是迟到、重复或取消后的
   `missing_callback`；internal ContinuationRef 缺失违反已认领消息的 continuation lifecycle，必须立即抛出
   `invalid_continuation`。只有不属于 async 协议/地址范围的消息才是 public `unhandled`。

## Error 与 exception 语义

1. 不得仅为了让上层再转换成异常而返回 `{error, Reason}`。校验错误和协议错误应在检测边界
   直接抛出。
2. 业务 Outcome 与编码错误必须分离。timeout、DOWN、manual abort 和业务
   `{error, Reason}` 仍是 continuation input，不能仅凭 tuple shape 将其分类为 runtime exception。
3. Callback 内部异常原样传播。不得宽泛捕获 `badfun`、`undef` 或业务异常，再将其改写为
   continuation availability failure。
4. 热升级后 current stored fun 本身不可调用属于 designed availability failure。只允许在唯一 apply
   boundary 捕获 term 精确相同的 `error:{badfun, CurrentFun}`，规范化为
   `{error,{sf_runtime,unavailable_continuation}}` 并只 dispatch direct parent；callback 内部或 downstream
   的 `badfun`/`undef` 仍原样传播。
5. Node 一旦消费，callback crash 或非法返回都不得恢复。直接异常传播必须保持
   take-before-invoke 与 exactly-once 语义。
6. Batch/active-failure 路径不得把 callback 协议错误改写为 status data；它应与正常执行路径
   一样直接异常退出。

## Message routing

1. `handle_message` 只消费已成功路由并恢复 continuation 后产生的 progress `{message, Payload}`，接口与
   message propagation 语义保持不变。
2. 精确路由接口必须区分 `{handled, Outcome, State}`、`{not_async, State}` 与
   `{missing_callback, Metadata, State}`。兼容接口可以把后两者映射为历史 `unhandled`，但内部不得因此
   丢失分类。
3. callback store 中合法的 continuation、accumulator或reverse-index entry不是action callback，精确路由返回
   `missing_callback`；action kind无法解码或value shape为`other`才是registry/invariant错误，应直接抛出。
4. Batch snapshot 中被先前 sibling cleanup 删除的 action 使用 `already_removed`，不得复用面向外部消息的
   `unhandled`。

## Callback 契约

1. Ordinary continuation callback `/1` 和 `async_r_t` sf callback `/2` 必须返回
   `async_r_t:async_r_t/3`。
2. Direct-state callback `/2` 的返回值被忽略。Direct-state callback `/3` 必须返回与输入 State
   具有相同 tuple tag 和 arity 的 State。
3. 初始化时无法通过检查 fun 证明返回协议。初始化只检查 fun identity、arity 和 mode；唯一
   invocation boundary 检查实际返回值，非法时立即抛出。不得使用伪造输入提前执行用户 callback
   进行校验。
4. Public API 与 constructor 的 `-spec` 必须明确展示 callback 返回契约，包括
   `async_r_t:async_r_t/3`。xref 只检查调用关系，不检查类型。Dialyzer 会读取这些 spec，但它是
   success-typing analyzer，不能保证静态发现所有被延迟保存的高阶 callback 类型错误。

## Review 拒绝信号

没有新的明确需求时，以下实现应被拒绝或简化：

- 在 dispatch、reply、message、take 或 fail 中执行递归 preflight 或全 store 扫描；
- 同一不变量在注册时检查后，又在每次执行时重复检查；
- `{error, Reason}` 只为稍后抛出而穿过多个 private layer；
- 同一违规存在多个 throwing wrapper 或不同 exception shape；
- 将 backend/热升级修复策略嵌入 continuation lifecycle；
- 为验证返回类型而提前执行 callback；
- node 消费后的 rollback 或 retry。

## 验证门槛

遵循这些原则的 runtime 重构应通过 compile、EUnit、完整 Common Test、xref、Dialyzer 和
`git diff --check`。测试应证明 public Outcome、exactly-once、cleanup ordering、message retention
和 exception identity，而不是绑定 private preflight helper 的内部结构。
