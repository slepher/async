# Coding Task Contract

- Task: task-coding-2
- Bundle generation: 1
- Phase artifact: task-coding-2.md
- Plan: plan-1.md
- Lane manifest: lanes-1.md
- Context: none
- Evidence target: task-coding-1 accepted merge on target branch async-sf-cc
- Task kind: coding
- Mode: hard
- Mode basis: converter call semantics, generated target validation, explicit precedence, designed incompatibility, and user exception propagation are one bridge/runtime boundary
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Lane: coding
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-migrate-g1-coding
- Base: task-coding-1 accepted merge on target branch async-sf-cc
- Depends on: task-coding-1
- Covers: none

## Bounded goal

Implement OpenSpec Task 2 on the accepted raw bridge: optional external `convert/2`, validated current Outcome/Env, same-identity explicit conversion precedence, sanitized incompatibility routed through the direct parent, and unmodified sf-cc user exception semantics.

## Normative objectives

- Accept only literal external `{Module, Function, 2}` converter MFA in a target descriptor; do not add HolderState, app Vsn, context, fun, or implicit converter generation.
- Call the converter with actual `OldOutcome` and saved `OldEnv`; accept only `{ok, NewOutcome, NewEnv}` or `{error, Reason}`.
- Generate/attach current target Outcome validation and require `NewEnv` to be an atom-key map containing every target required capture; allow extra keys.
- Use an exact mapped converter bridge even when identity and capture names are otherwise automatically compatible. Once a Step mapping matches, any plan/converter/output failure is terminal for that explicit path and must not fall back.
- Normalize converter rejection, invalid result, incompatible Outcome, incompatible Env, and invalid historical frame to the specified bounded `incompatible_frame` information, then reuse existing direct-parent structural failure forwarding.
- Never include Outcome, Env, State, callback values, funs, stack traces, or unrelated internals in failure metadata. If business Reason is retained as controlled detail, document and bound that interface.
- Converter/router `error`, `exit`, and `throw` remain user-code exceptions under existing sf-cc semantics; do not catch them as designed incompatibility.

## Owned paths

- `src/async_sf_transform.erl`
- `src/async_sf_runtime.erl`

All test, README, other production, OpenSpec, and workflow paths are read-only. Work starts only from the target-visible accepted task-coding-1 merge.

## Preserved invariants

- Raw bridge and automatic fallback behavior accepted in task-coding-1 remain unchanged for mappings without converters and Steps absent from a plan.
- Bridge remains reply-time and one-shot; no frame replacement, State/callback scan, request replay, or parent dispatch occurs inside generated code.
- Existing `async_sf_cc` direct-parent routing and user callback exception behavior remain the sole lifecycle/exception authority.
- Failure identity is stable semantic metadata separated from presentation; sensitive runtime values never become diagnostics.

## Implementation simplicity

- Standard: audit-implementation-simplicity
- Task-specific requirements: extend the existing bridge descriptor and runtime result path rather than adding a converter service/module or second error channel; store only external MFA and minimum target metadata; generate validators at the layer already owning current pattern/capture semantics; represent designed converter failures as one canonical internal result consumed once by runtime; catch no converter exception; preserve native exception class/cause/stack; use one linear path convert-or-reuse -> validate target -> current Step, with no immediate wrap/unwrap or duplicate return/state flag.
- Required evidence: converter MFA callers and return-use matrix; old input -> converter/raw -> target validator -> current Step representation trace; designed error versus runtime exception table; sanitized Info key/value audit using sensitive sentinel inputs; explicit-over-automatic call trace; no-fallback proof for matched invalid mappings; owned diff, omission search, and validation exits.

## Ordered work

1. Extend literal mapping validation and frozen bridge metadata with the optional external MFA; preserve raw mapping rules unchanged.
2. Generate the minimum target Outcome matcher and Env validation metadata/code from the accepted current boundary analysis.
3. Implement linear converter/raw bridge execution and canonical designed-failure result; external-call the current generated Step only after target validation.
4. Extend runtime exact-plan validation and incompatibility normalization, keeping explicit mapping precedence and direct-parent forwarding.
5. Reread all bridge/router/runtime callers for duplicate failure channels, sensitive metadata, broad catches, fallback-after-match, or representation churn.

## Blocking self-validation

- Commands: `rebar3 compile`; `rebar3 xref`; `rebar3 eunit --module=async_sf_transform_tests`; `git diff --check`; bounded `rg` audits for converter catches, duplicate incompatibility channels, payload leakage, State/context, and silent automatic fallback

## Deferred downstream validation

- Commands: Outcome-only, Env-only, combined, same-identity, missing-converter, converter rejection/invalid output/user-exception, failure-handler, privacy, and linked-callback Common Test scenarios
- Owner: task-test-1
- Expected transitional failures: no existing-suite failure is accepted; the coding candidate defers only independent black-box converter/failure coverage and real linked callback evidence to task-test-1
- Closure gate: task-test-1 must cover task-coding-2 on the same target-visible candidate, prove designed/error/exception boundaries and privacy, pass full validation, and route every production finding through coding -> target -> integrate

## Observable completion

- Actual old `(Outcome, Env)` can be converted once and passed to the exact current Step; same-identity explicit conversion wins over automatic inheritance.
- Every specified designed failure reaches only the current node's direct parent with bounded sanitized metadata, while converter user exceptions are not swallowed or reclassified.
- No new context/state channel, service abstraction, callback mutation, reverse converter, or fallback-after-explicit-failure exists.

## Stop conditions

- task-coding-1 is not accepted and visible on target, or its plan/bridge contract differs materially.
- Specification cannot determine target Outcome validation or controlled business Reason representation.
- Correct behavior requires editing sf-cc lifecycle owners or another unowned production path.
- Blocking checks expose an unowned baseline/environment failure or overlapping unrelated change.

## Handoff

Return `Status`, exact candidate, owned diff/symbols, converter representation and exception evidence, privacy/no-fallback audits, command exits, deferred evidence, and risks. Do not edit tests/docs/specs/workflow, stage/commit/merge, or start task-coding-3.
