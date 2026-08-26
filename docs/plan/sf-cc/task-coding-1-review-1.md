# Coding Review

Status: complete

Task: task-coding-1

Reviewed commit: 30d29c3d7ecc4da96e1052b8066c43a4298aee76

Previous reviewed commit: none

Evidence:

- immutable candidate 的唯一 parent 为 `9691772e7e260e32cb5c7fd11c0cd51563b3530a`，diff 恰好只包含 contract 列出的 12 条 owned paths；未以 moving worktree 或未提交 delta 作为证据。
- callback encoding 只有 `include/async_internal.hrl` 的一个 record owner；`async_util:decode_callback/1` 是三元/四元 tuple 的唯一 decoder，`callback_exists/1`、routing、timeout、cleanup、inspection 都通过 decoder/classifier 读取，旧三元 tuple 明确得到 `sf_cc_mref => undefined`。
- `async_r_t` 用独立 ReaderT 层传播 AccRef/CcRef；`local_ref/3` 仍只改变 AccRef，`local_sf_cc_ref/3` 独立改变 CcRef；`async_t:promise/3` 将当前 CcRef 写入 callback 尾字段，没有 sidecar mapping。
- `async_sf_cc` 是 descriptor、CcRef entry、completion、inspection 与 fail 的唯一 owner；`async_sf_runtime` 仅解析/执行 frame 并委托 `async_sf_cc:dispatch/3`、`complete/3`，旧 `invoke_sf_cc`、`terminal_cc`、`remove_ref_after_cc` 均不存在。
- `async_sf_cc:terminal/1` 只产生精确的 `#{kind => sf_cc_ref, protocol => 1, mref => CcRef}`；terminal 不包含 callback、Context 或 Env。`async_sf_runtime` 每个 frame 通过自身 `module` 独立查询 current manifest，transform 保存 literal `vsn`/`module`，没有引入 sf-migrate 的 catalog、migrator、composition 或 downgrade policy。
- inspection summary 只含 request/CcRef protection、continuation kind 与 sanitized frame metadata；matcher 在 commit 前对排序后的 summaries 各调用一次，fail 对 protected CcRef 去重并按 term order commit。ordinary runner/wait 复用同一 descriptor/completion core，内部并行组合使用窄 `raw_run_cc/3` 避免额外 final owner。
- dispatcher 提供的 mechanical evidence 为：compile exit 0；`async_t_SUITE` 25/25；EUnit 12/12；xref 0；manual API/macro/matcher probes passed；`async_sf_SUITE` 仅 5 个已知旧 fixture 通过、5 个旧 terminal fixture 失败。代码与 contract evidence 证明后 5 项只断言已退役的内嵌完整 `sf_cc` terminal / 单 callback-ref shape，因此按用户授权分类为 `task-test-1` 已拥有的下游 fixture migration gate，不构成 coding worker 修改 `test/` 的依据。

Findings:

- `F1`（blocking，R9/R10）：commit `30d29c3d7ecc4da96e1052b8066c43a4298aee76` 的 `src/async_sf_cc.erl:120-132` 在 `prepare/5` 中显式从 orphan cleanup 排除 `ActiveAccRef`；`src/async_sf_cc.erl:213-217` 随后把 `invoke(SfCc, ...)` 作为 `then/3` 的已求值参数构造，并只在 callback effect 返回后调用 `cleanup_acc_ref/2`。Erlang 参数求值会使 async/legacy 分支在 clean state 的 `do_put_state` 执行前进入 `src/async_sf_cc.erl:180-188` 的用户 callback；当 suspended action 使用过 local accumulator 时，该 accumulator 也仍存在直到 callback effect 完成。这违反 contract R9 的 preflight → take → owned lifecycle/accumulator cleanup → invoke 顺序，也使 worker 声称的 cleanup-before-invoke 尚未由实现成立。

Verdict: changes_required

Changes:

- Correction owner: coding。仅修改 task-owned `src/async_sf_cc.erl` 的 `prepare/5`、`invoke_prepared/5` 及必要的同模块 helper：在 descriptor preflight/take、owned action/resource cleanup 后，只要没有 surviving callback 引用，就把 `ActiveAccRef` 与其他 removed AccRef 一并从 clean callbacks 删除；先把该 clean state 提交给 state transformer，再通过 `bind` continuation 延迟构造并调用 async/legacy user callback，最后执行其返回 effect，且 effect 的 CcRef scope 仍为 `undefined`。删除或收窄已经不再承担 lifecycle ownership 的 post-invoke `cleanup_acc_ref/2` 路径。必须保留 unrelated callbacks、仍被 surviving callback 引用的 shared accumulator、message 不消费、direct `/2|3` return/state compatibility、old tuple fallback，以及 callback 自己新建的后续 state/effect。
- Focused closure：增加或运行一个 suspended candidate probe，使 action 先写入 local accumulator，再让 final callback 的第一步检查 state；在任何 callback 代码/effect 前，CcRef entry、owned action/resource 与无 surviving reference 的 accumulator 均须不存在。另验证 shared accumulator 有 unrelated surviving callback 时保留、callback crash 后 CcRef 不恢复/不重试、ordinary result shape 不变；重跑 compile、`async_t_SUITE`、EUnit、xref。5 个旧 `async_sf_SUITE` fixture 继续作为 `task-test-1` 下游门槛，不要求 coding correction 修改 `test/`。

Caveats:

- impossible evidence-boundary repair 已成立：5 个已知旧 terminal fixture 失败已分类为 `task-test-1` 的下游迁移门槛；本 review 未把它们当作产品 finding，也未要求 coding worker 修改 `test/`。
- 本 review 未修改任何产品或 test path，仅写入本 immutable review artifact；未 stage、commit，也未修改 status、ledger、plan、lanes、OpenSpec 或 checklist。
