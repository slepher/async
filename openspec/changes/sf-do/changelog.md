# Changelog

<!-- NEXT_CHANGE: 在此标记下方、分隔线上方追加后续变更，最新内容置顶。 -->

## 2026-08-26 — 未实现（合同已更新）

- `sf-do` 不再拥有或定义完整 `sf_cc` descriptor、public final runner、callback arity/return
  contract 或 final invoker；这些能力统一由 `sf-cc` change 提供。
- stable continuation chain 的 terminal 从内嵌完整 `sf_cc` 改为
  `#{kind => sf_cc_ref, protocol => 1, mref => CcRef}`。完整 external callback、mode 和预存
  Context 只保存在 CcRef entry 中。
- stable action callback 继续在 `#callback.cc` 保存原 frame continuation，同时通过新增尾字段
  `#callback.sf_cc_mref` 保存最终 CcRef；旧三元 callback tuple 按
  `sf_cc_mref = undefined` 兼容读取。
- compatibility boundary 改为 callback 创建时间，而不是 runner 名称：升级后由 `wait`、ordinary
  anonymous callback runner 或 sf-cc runner 新启动的 Promise 都创建 CcRef；只有升级前已经 pending
  的旧三元 tuple 保持 unprotected。
- `wait/1`、`wait_t/2` 保持原 API 和正常返回，由库内固定 external completion 无感迁移到 CcRef
  engine。旧 anonymous callback API 继续支持原 source/return contract，并通过 legacy adapter 使用
  统一 lifecycle；只有显式 external callback 获得 final callback 本身的跨升级保证。
- old unprotected tuple 的 anonymous continuation 若因旧代码 purge 不可调用，runtime MUST 清理该
  request 且不得 crash/retry。因为没有 CcRef，不调用 final CC；`async_m:handle_info` 返回
  `{legacy_outcome, Outcome, CleanState}`，其中 Outcome 是原本要交给旧 continuation 的 action Outcome，
  由 receive-loop 调用方决定如何处理。
- 同一 Promise 的 nested/parallel internal callbacks 可以继承同一 CcRef，但保持独立 RequestMRef；
  共享 CcRef 不是多 Promise operation group，也不产生 per-entry final callback。
- structural frame failure 不再递归遍历可能损坏的 continuation 寻找 CC；runtime 必须通过
  `#callback.sf_cc_mref` 进入 `sf-cc` 的统一 completion primitive，由它先消费并清理 CcRef
  所属 lifecycle，再调用 final callback。
- `sf_frame` 和 current manifest 增加 owner Module 与 module `-vsn` literal term（无可用显式
  Vsn 时为 `undefined`）。Vsn 不进入 AsyncId，只供 inspection 和 migration endpoint selection。
- frame resume 增加逐节点 owner-module resolution 边界。外部 owner module 独立升级、持有进程未执行
  `code_change/3` 时，reply-time 仍须在调用 frame MFA 前进行 lazy resolve；不同模块的 nested frames
  分别处理。
- `sf-do` 只提供 source identity、stored Env、frame owner/version 和 current target manifest。
  historical block、Env/Input migrator、migration catalog、version-chain composition 与 strict downgrade
  policy 由 `sf-migrate` 定义，不在本 change 重复冻结。
- ordinary timeout、DOWN、message、Monad failure、source syntax、BlockPath/StepName identity、capture
  analysis 和 nested Monad composition 保持既有合同。

### 实现核实

截至 2026-08-26，仓库实际情况如下：

**已实现的旧基线**

- `async_sf_transform` 已生成 `bind_frame`/`then_frame`、external generated frame MFA、minimal Env、
  BlockPath/StepName/AsyncId 和 `'$async_sf_manifest'/0`。
- `async_sf_runtime` 已实现 `run_sf_frame/4`、内嵌完整 `sf_cc` terminal、current-manifest validation、
  frame resume 和 structural failure 后调用旧 terminal `sf_cc`。
- `async_t` 已实现 frame-aware bind/then 和在 `#callback.cc` 中保存 stable continuation data。

**尚未实现的新合同**

- 尚无 `#callback.sf_cc_mref`、CcRef entry、`sf_cc_ref` terminal 或旧/新 callback tuple 统一 decoder。
- 尚无 `run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5` external-fun macro/runtime 和统一
  cleanup-before-invoke completion primitive。
- 现有 `wait/1`、`wait_t/2` 和 ordinary runner 尚未迁移到 CcRef engine；`handle_info/3` 也尚无
  `{legacy_outcome, Outcome, CleanState}` 分支或 unavailable legacy fun 的受控清理。
- frame/manifest 尚未保存 module Vsn，inspection matcher 也尚未暴露 Module/Vsn/frame summaries。
- frame resume 仍直接校验/调用 current manifest MFA，尚无按 owner module/version 的 lazy migration
  resolution、optional Input conversion 或无 historical block 的 upgrade path。
- 因此本节上方 2026-08-26 的条目是待实现目标，不表示当前 runtime 已满足新合同。

---

## 2026-08-24 — 已实现

- `sf_do` public source 收敛为 `sf_do(Comprehension)` 与
  `sf_do(BlockName, Comprehension)`；StepName 改为紧邻非最终 action 的 inline literal atom
  marker，不再使用额外 list/record 参数。
- standalone literal atom qualifier 只允许作为 StepName marker；duplicate、dangling、连续 marker、
  修饰 pure let/final action 或其他位置均为 source error。unnamed boundary 产生 warning，允许普通
  执行和 same-version resume，但 appup/appdown migration 必须失败。
- migration identity 只使用 Protocol、BlockPath 和 StepName；删除 derived identity、Action
  canonicalization、AsyncForm digest 及 AST/index fuzzy matching。
- Pass 1 使用 Astranaut return channel 累积 errors/warnings；有 error 时直接
  `astranaut_return:to_compiler/1` 且不运行 Pass 2，只有 warning 时继续 rewrite。Pass 2 只消费冻结
  plan，不重复 source validation。
- scope-aware capture 明确区分 `case`/`receive`/`try` outer equality reference 与
  `fun`/named-fun fresh binding，并要求共享 lexical binding classifier。
