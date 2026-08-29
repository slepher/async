# Changelog

<!-- NEXT_CHANGE: 在此标记下方、分隔线上方追加后续变更，最新内容置顶。 -->

## 2026-08-28 — precise async routing and availability boundary（未提交）

- async info routing新增精确分类：`handled | not_async | missing_callback`；legacy `run_info`/`handle_info`
  继续兼容历史`unhandled`。
- external ActionMRef缺失不再与internal ContinuationRef缺失混用：前者是`missing_callback`，后者直接抛出
  `invalid_continuation`；active-fail sibling cleanup使用`already_removed`。
- ordinary outer continuation直接保存真实callback，不再藏在通用`sf_cc` context wrapper后；purge导致的
  term-exact current-fun `badfun`规范化为`sf_runtime unavailable_continuation`并只forward direct parent。

## 2026-08-28 — unified ContinuationRef chain（未实现）

- stable frame inspection只暴露Protocol/Block/StepName/MFA identity metadata；删除BlockPath与AsyncId投影，
  named generated MFA由sf-do固定为`'$sf_<block>__<step>'/2`。
- 将 continuation edge 字段统一命名为 `parent_ref`：它表示 current `cc`/frame 的 direct dynamic
  continuation parent，不表示下一 source qualifier，也不表示 resource owner。
- ordinary action callback 改为 `#callback{cc,acc_ref,parent_ref,...}`。classic `do` bind 不拆包 Promise；
  真正挂起的 Promise writer从显式 `SfContext` 保存 current anonymous `cc` 与 direct parent Ref。
- ordinary Outcome仍调用 `cc`；只有可精确归属到 current `cc` 本身不可调用的 availability failure才跳过
  `cc` 并 dispatch `parent_ref`。因此无需为每个 ordinary action再包装一个重复的 continuation node。
- 移除 `FinalCcRef`/`OuterRef` 的类型和生命周期区分。callbacks store 中所有 continuation references
  使用同一种 Ref；Ref 指向 frame、ordinary callback 或 completion descriptor node。
- ordinary/stable action node 都保存当前 continuation 和直接 `parent_ref`。不再用
  `#callback.sf_cc_mref` 在所有 pending
  entries 中复制 root，也不再以 `sf_cc_ref` map 作为 structured chain terminal。
- dispatch 改为 take/validate/execute 一个节点：success 执行当前 continuation；failure 跳过普通 bind
  success frame并转发给 `parent_ref`；节点调用产生的可归属 failure 同样只向外跳一层。
- final completion 是 `parent_ref = none` 的最外层 callback node 行为，不是另一种 reference。exactly-once
  从“按 root CcRef 批量消费”改为“每个 node 在调用/转发前一次性消费”。
- inspection、主动 failure、timer/monitor cleanup 和 parallel cancellation 改以节点链及独立反向索引工作；
  不允许靠每层重复 root Ref 恢复旧的 ownership 模型。
- 本条 supersede 2026-08-27/26 中关于四元 `#callback.sf_cc_mref`、`sf_cc_ref` terminal、按 CcRef
  deduplicate/cleanup 和 structural failure 直达 final completion 的合同。

## 2026-08-27

- continuation context 改为显式 `SfContext` 参数，不再通过进程字典传播 stable continuation/runner。
- ordinary bind 继续使用 anonymous continuation；只有 `bind_frame/4` 创建并保存 `sf_next` frame。
- Promise 仅在实际 continuation 等于 stable runner 时保存 structured continuation；`bind_frame` 位于
  ordinary bind 下方时，由独立 CcRef descriptor 承接 anonymous downstream。
- 删除 `legacy_cc`/legacy mode、`sf_legacy_next`、旧三元 callback tuple decoder 和
  `{legacy_outcome, Outcome, CleanState}` compatibility path。当前 callback store 只接受四元 record。
- ordinary runners 和 wait 仍使用统一 CcRef lifecycle，但 ordinary completion adapter 编码为普通
  `sf_cc` descriptor，不再冻结独立 legacy descriptor 类型。
- ordinary action continuation 的 `badfun` 属于 ordinary bind 执行错误，原样传播；sf-cc 不负责把它
  转换为 terminal Outcome。final callback availability 仅在实际 apply 边界进行精确处理。
- matcher exception 和 callback/effect 内部异常原样传播；不再包装 Class/Stacktrace。主动失败仅把预期的
  unavailable callback 或 invalid return 汇总为无错误载荷的 `cc_failed` status。
- 本条 supersede 2026-08-26 条目中的 old-tuple、legacy adapter/outcome 和宽异常汇总合同；旧条目仅作为
  当日历史设计记录保留。

---

## 2026-08-26

- 建立独立 `sf-cc` capability，以一次性 CcRef 统一 legacy anonymous promise 和
  stable frame promise 的最终 completion。
- 新增 `run_sf_cc/3`、`run_with_sf_cc/5` 和 `exec_sf_cc/5` 命名边界；旧
  `run_cc`/`run_with_cc`/`exec_cc` 与 `callback_to_cc` 保持原参数、normalization 和正常返回合同。
- 冻结统一 adoption 边界：升级后 `wait`、ordinary runners 与 sf-prefixed runners 都创建 CcRef；
  ordinary APIs 通过 legacy final adapter 接入，不受 external-fun macro constraint。
- `sf_cc` 保存用户显式 external fun、预存 Context 和 runner mode；Astranaut macro
  在编译期拒绝 local/anonymous fun 及 mode 不支持的 arity。
- `#callback{}` 尾部最小增加 `sf_cc_mref`；新 runtime 显式兼容升级前的
  `{callback, Cc, AccRef}` tuple，但不伪造缺失的 CcRef。
- 升级前旧三元 tuple 的 anonymous continuation 若因 purge 已确认不可调用，清理可识别 request lifecycle，
  由 `async_m:handle_info/3` 返回 `{legacy_outcome, Outcome, CleanState}`；不 crash、不 retry、
  不合成 orphan Outcome，也不调用不存在的 final CC。
- final completion 先消费 CcRef 并清理 action lifecycle，再以 CleanState 调用用户
  `sf_cc`；迟到 reply 不得二次调用。
- 新增 function matcher 驱动的 inspect/immediate-fail 能力，公开 Module/Vsn/Protocol/Block/StepName
  等只读 metadata，不公开 Env、State 或 CC Context。
