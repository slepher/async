# Changelog

<!-- NEXT_CHANGE: 在此标记下方、分隔线上方追加后续变更，最新内容置顶。 -->

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
- 新增 function matcher 驱动的 inspect/immediate-fail 能力，公开 Module/Vsn/BlockPath/
  StepName 等只读 metadata，不公开 Env、State 或 CC Context。
