# Tasks

## 1. Shared callback representation

- [ ] Move the internal callback record to one shared header and append
  `sf_cc_mref = undefined` without changing `cc` or `acc_ref` semantics.
- [ ] Add a single old/new callback decoder for `{callback, Cc, AccRef}` and
  `{callback, Cc, AccRef, SfCcMRef}`.
- [ ] Replace direct record-pattern readers in `async_t`, `async_util`, sf runtime, migration enumeration and tests with the
  decoder/classifier.
- [ ] Keep old three-tuples resumable as unprotected callbacks; never infer a CcRef from their continuation. When a purged
  old anonymous continuation cannot be entered, clean the identifiable request lifecycle and return
  `{legacy_outcome, Outcome, CleanState}` from `async_m:handle_info/3` without invoking a final CC.
- [ ] Update `callback_exists/1` to count only decoded action callbacks.

## 2. sf-cc macro API

- [ ] Add an Astranaut provider for `run_sf_cc/3`, `run_with_sf_cc/5` and `exec_sf_cc/5` and enable it from the public
  include independently of `async_sf_transform`.
- [ ] Accept only explicit remote/external fun AST; preserve the fun expression in the generated ordinary runtime call.
- [ ] Restrict `run_sf_cc/3` to callback `/2`; restrict the direct runners to callback `/2|3`.
- [ ] Accumulate source-position diagnostics through Astranaut return values and provide formatter messages.
- [ ] Add compiler tests for external, local, anonymous, variable/expression and wrong-arity inputs.

## 3. Descriptor and CcRef store

- [ ] Add `async_sf_cc` types, constructors and validators for `sf_cc` and `sf_cc_ref` protocol `1` maps.
- [ ] Store the original external-fun term, opaque Context and explicit runner mode; do not store an MFA tuple.
- [ ] Add callbacks-dictionary classification so reply routing ignores CcRef and accumulator entries.
- [ ] Ensure standalone CcRef entries do not make `callback_exists/1` report a pending action.
- [ ] Cover map, orddict/list and dict callback backends.

## 4. Independent AccRef/CcRef scope

- [ ] Introduce a backward-compatible reader-local scope containing `acc_ref` and optional `sf_cc_mref`; accept old raw Ref
  reader values.
- [ ] Keep `get_local_ref` public behavior unchanged and add internal `get_sf_cc_mref`.
- [ ] Make `local_ref/3` replace only AccRef and preserve CcRef.
- [ ] Rebuild scope from both fields before executing a resumed action callback.
- [ ] Copy current CcRef into every newly suspended callback while leaving legacy fun/frame data unchanged.
- [ ] Test sequential suspension and internal AccRef rebinding/parallel aggregation.

## 5. Stable runners

- [ ] Implement ordinary runtime targets and public facades for `run_sf_cc/3`, `run_with_sf_cc/5`, `exec_sf_cc/5`.
- [ ] Allocate/register CcRef at effect execution time, install sf-cc scope and route normal final Outcome through the common
  completion primitive.
- [ ] Move `wait/1`, `wait_t/2`, `run_cc/2`, `run_with_cc/4`, `exec_cc/4` and `run/4` onto the common CcRef core.
- [ ] Preserve existing ordinary callback normalization and normal return behavior through a legacy final descriptor; use a
  library-owned exported completion for wait and do not impose the sf macro source constraint on ordinary APIs.
- [ ] Test immediate and suspended legacy Promises through all sf-cc runner modes.

## 6. Exactly-once completion

- [ ] Implement preflight + take CcRef + owned callback/lifecycle cleanup + invoke ordering.
- [ ] Pass CleanState to direct callback `/3`; ignore direct callback `/2` return; require `async_r_t` from async callback `/2`.
- [ ] Validate direct `/3` NewState using existing State type compatibility and reject `async_r_t`.
- [ ] Keep CcRef consumed after callback crash, invalid return or effect failure; never rollback/retry.
- [ ] Make late reply, duplicate completion and duplicate fail unhandled without a second callback invocation.
- [ ] Preserve unrelated callbacks/resources and safely remove shared accumulator refs only when unreferenced.

## 7. Frame integration

- [ ] Replace embedded full `sf_cc` chain terminal with `#{kind => sf_cc_ref, protocol => 1, mref => CcRef}`.
- [ ] Require terminal mref and callback.sf_cc_mref consistency.
- [ ] Route normal terminal completion and structural/directional failure through `async_sf_cc:complete`.
- [ ] Use decoded callback field directly on damaged continuation instead of recursively locating final callback.
- [ ] Update nested frame construction, validation, stripping, fixtures and kind walkers.
- [ ] Add frame owner module and optional literal module Vsn to sanitized frame summaries without exposing Env.

## 8. Inspection and immediate failure

- [ ] Implement callback-level `inspect_sf_callbacks/3` with one matcher call per action callback and current-to-downstream
  frame summaries.
- [ ] Expose request MRef, optional sf_cc_mref, protection/kind, Module/Vsn/protocol/BlockPath/StepName/AsyncId only.
- [ ] Normalize callback order and make matcher exception/non-boolean return a no-mutation error.
- [ ] Implement `fail_sf_callbacks/4` with full matcher preflight, unprotected results, CcRef deduplication and sorted commit.
- [ ] Use `{error, {sf_aborted, Reason}}`; catch/report per-CcRef CC failures after consumption and continue the batch.
- [ ] Test privacy, nested multi-module matching, matcher atomicity, dedupe, already-consumed and unprotected entries.

## 9. Legacy and lifecycle regression

- [ ] Prove old three-tuple callbacks survive upgrade and reply normally.
- [ ] Prove old tuples remain unprotected and are not deleted by active failure.
- [ ] Test reply, message, timeout and DOWN routing with protected legacy and frame callbacks.
- [ ] Test purged/unusable protected legacy fun fallback through its CcRef without calling the continuation.
- [ ] Test purged/unusable pre-upgrade old tuple cleanup and tagged `handle_info/3` return without CcRef/final CC.
- [ ] Prove the unavailable-code classifier does not swallow callback business exceptions or nested `undef`.
- [ ] Test callback external side effect followed by crash does not cause retry.
- [ ] Document rollback requirement for new four-tuple callbacks before loading runtime code that only understands old tuples.

## 10. Documentation integration

- [ ] Make `sf-do` depend on `sf-cc`; remove the old full-descriptor terminal owner from sf-do proposal/design/spec/tasks.
- [ ] Make `sf-migrate` terminal mismatch/abort use `#callback.sf_cc_mref` and `async_sf_cc:complete` without redefining final
  callback semantics.
- [ ] Document ordinary Callback -> anonymous `CC/1` adaptation separately from external Callback -> stable `sf_cc` data.
- [ ] Document Context as opaque pre-saved data and `/3` State as CleanState.
- [ ] Document that every post-upgrade runner creates CcRef while only sf-prefixed source forms require an external fun.

## 11. Verification

- [ ] `rebar3 compile`.
- [ ] `rebar3 eunit`.
- [ ] `rebar3 ct`.
- [ ] `rebar3 xref`.
- [ ] `rebar3 dialyzer`.
- [ ] Run supported OTP compatibility matrix, including a real old three-tuple State fixture.
- [ ] Validate the OpenSpec change and resolve every requirement/task inconsistency before implementation starts.
