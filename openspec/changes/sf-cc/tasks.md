# Tasks

> 2026-08-28：第 1–5 节中四元 callback/root `sf_cc_mref`、recursive `sf_next`、`sf_cc_ref`
> terminal、按 root CcRef cleanup/dedupe 和 structural failure 直达 final 的已完成项，已被第 7 节
> unified ContinuationRef contract supersede；check 状态仅保留历史实现事实。

## 1. Callback representation and CcRef store

- [x] Keep the current four-tuple callback record encoding in one internal shared header.
- [x] Decode only the current callback encoding; do not add old three-tuple compatibility.
- [x] Classify action callbacks, CcRef entries and accumulator data independently.
- [x] Make `callback_exists/1` count only decoded action callbacks.
- [x] Store `sf_cc` and `sf_cc_ref` protocol `1` descriptors in the existing callbacks dictionary.

## 2. Public runners and macros

- [x] Provide Astranaut validation for `run_sf_cc/3`, `run_with_sf_cc/5` and `exec_sf_cc/5`.
- [x] Accept only explicit external fun source forms with the supported `/2|3` arities.
- [x] Preserve ordinary `run_cc`/`run_with_cc`/`exec_cc`/`run` and wait contracts through the common CcRef core.
- [x] Encode ordinary completion adapters as normal `sf_cc(mode = async_r_t)` descriptors; do not add a legacy mode.

## 3. Explicit continuation context

- [x] Pass `(K, SfContext)` explicitly through `async_t`; do not use process dictionary state.
- [x] Keep ordinary bind continuations anonymous and make `bind_frame/4` the sole `sf_next` constructor.
- [x] Save structured continuation only when the actual continuation equals `SfContext.runner`.
- [x] Bridge an ordinary downstream below `bind_frame` through an independent CcRef descriptor.
- [x] Keep AccRef and optional CcRef in independent `async_r_t` reader locals and rebuild both on resume.

## 4. Completion and failure boundaries

- [x] Consume CcRef and clean owned callbacks/resources before final callback invocation.
- [x] Preserve CleanState and never rollback or retry a consumed CcRef.
- [x] Validate direct `/3` State and async `/2` effect return shapes.
- [x] Precheck external final callback availability with `ensure_loaded`/`function_exported`.
- [x] Catch only exact `error:{badfun, Callback}` at the final callback apply boundary.
- [x] Propagate ordinary action callback, nested callback, effect and matcher exceptions unchanged.
- [x] Remove Class/Stacktrace failure payloads and legacy outcome wrappers.

## 5. Frame integration and active failure

- [x] Terminate stable chains with `sf_cc_ref` and keep the full descriptor solely in the CcRef entry.
- [x] Route stable-frame structural/directional failure through the CcRef completion primitive.
- [x] Expose sanitized `ordinary_fun | sf_frame | unknown` callback summaries.
- [x] Preflight all matcher calls before mutation; propagate matcher exceptions and reject non-boolean returns.
- [x] Deduplicate selected protected callbacks by CcRef and report `completed | cc_failed | unhandled`.
- [x] Keep current callbacks without a CcRef as `unprotected` without deleting them.

## 6. Documentation and verification

- [x] Synchronize the sf-cc and sf-do OpenSpec contracts with the explicit context and narrowed error ownership.
- [x] `rebar3 compile`.
- [x] `rebar3 eunit`.
- [x] `rebar3 ct`.
- [x] `rebar3 xref`.
- [ ] Resolve the existing project-wide Dialyzer warnings before claiming a clean Dialyzer pass.
- [x] Validate both OpenSpec changes after the documentation update.

## 7. Unified ContinuationRef rewrite（2026-08-28，未实现）

- [ ] 在 shared header 定义 ordinary `#callback{cc,acc_ref,parent_ref,...}` 与 stable
  `#sf_callback{frame,acc_ref,parent_ref,timer_ref}`，并统一所有 store readers/classifiers；不保留缺少 `parent_ref` 的旧
  callback tuple reader。
- [ ] 用统一 ContinuationRef node替换 `sf_cc_ref` entry/terminal；Ref value支持 stable frame、ordinary
  continuation和outer sf_cc descriptor。
- [ ] 让 ordinary `bind` 显式透传 `SfContext.parent_ref`，由真正挂起的 Promise writer保存
  `cc + parent_ref`；实现 `bind_frame` direct-parent materialization和single-frame suspension writer。
- [ ] 实现 take-before-dispatch逐节点 executor；success运行当前 continuation，failure跳过普通 success
  frame并只dispatch direct `parent_ref`。
- [ ] 删除 root `sf_cc_mref` reader local、per-callback propagation、root scan cleanup和per-CcRef fail dedupe。
- [ ] 建立仅供 active cancellation/parallel cleanup使用的reverse index/ownership metadata，并验证registration、
  take、shared parent和orphan cleanup一致性。
- [ ] 更新 exception boundary，只转换精确归属于current continuation availability的 `badfun`/`undef`。
- [ ] 更新 inspection/failure API为current-node summary、可选安全Ref traversal和
  `forwarded | handled | failed | unhandled | ordinary` results；ordinary action的`parent_ref`只用于current
  `cc` availability fallback，主动failure保持pending并返回`ordinary`。
- [ ] 更新 runner、message/timeout/DOWN、late reply和所有 callback-store backend tests。
- [ ] 运行 compile、EUnit、Common Test、xref、Dialyzer和三组 OpenSpec validation；不得以旧测试通过标记
  本节完成。
