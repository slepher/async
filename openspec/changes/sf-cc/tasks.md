# Tasks

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
