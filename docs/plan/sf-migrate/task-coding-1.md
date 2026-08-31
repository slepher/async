# Coding Task Contract

- Task: task-coding-1
- Bundle generation: 1
- Phase artifact: task-coding-1.md
- Plan: plan-1.md
- Lane manifest: lanes-1.md
- Context: none
- Evidence target: aa7c6cd4f6b79659b1293b753a7aa5dd2f98cf41
- Task kind: coding
- Mode: hard
- Mode basis: exact historical source analysis, generated plan/bridge metadata, and runtime validation/selection are one shared representation contract
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Lane: coding
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-migrate-g1-coding
- Base: aa7c6cd4f6b79659b1293b753a7aa5dd2f98cf41
- Depends on: none
- Covers: none

## Bounded goal

Implement OpenSpec Task 1 as one working vertical slice: exact literal source Vsn/block declarations containing complete historical named sf-do, raw StepName mapping and compatibility checks, version-specific bridge/opaque plan generation, and reply-time explicit-plan selection before the existing automatic named fallback.

## Normative objectives

- Recognize `sf_migrate(OldVsn, CompleteOldComprehension, #{from := OldBlock, to := CurrentBlock, steps := Steps})` only on `sf_do_code_change/2` return paths and auto-export the router.
- Require an explicit FrameOwner `-vsn`; preserve exact literal terms without coercion, ordering, predecessor inference, graph, or multi-hop.
- Reuse the current sf-do qualifier, marker, pattern, lexical capture, and tail analysis for historical input; emit metadata/bridge inputs but no historical workflow functions, requests, exports, or manifest AST.
- Resolve literal old/current Block and Step names exactly; reject partial/unnamed historical/current blocks, duplicate source declarations, invalid placement/shape, missing targets, and generated collisions with the specified bounded diagnostics.
- Without converter, require equal normalized Outcome shape and target captures as a subset of historical source capture names.
- Lower each declaration to one opaque internal plan and each mapped Step to a collision-safe version-specific raw bridge that calls the current generated Step `/2` with the actual Outcome/Env.
- In `async_sf_runtime:continue/5`, use one current manifest snapshot. Same Vsn stays on the existing path; different Vsn asks the current owner router, validates exact plan metadata, uses a mapped bridge first, otherwise tries existing named identity/capture-subset resolution.
- Never rewrite/store a frame, enumerate callbacks, run old qualifiers, resend a request, dispatch a parent from the bridge, or add another manifest.

## Owned paths

- `src/async_sf_transform.erl`
- `src/async_sf_runtime.erl`

All `test/`, `include/`, other `src/`, README, OpenSpec, and workflow paths are read-only. If a correct implementation requires another product path, stop and report the exact caller/ownership reason.

## Preserved invariants

- Named identity remains `{1, Block, StepName}` and Vsn remains metadata, never identity.
- Current `'$async_sf_manifest'/0` shape remains authoritative and contains no historical AST or migration catalog.
- Saved Env extra atom keys remain allowed; ordinary/unnamed/same-version behavior is unchanged.
- `async_sf_cc` remains the only callback take, direct-parent routing, timer/monitor/accumulator, and cleanup owner.
- Router absence, `unhandled`, and an unmapped Step are normal explicit-plan misses; a malformed matching plan is not silently accepted.

## Implementation simplicity

- Standard: audit-implementation-simplicity
- Task-specific requirements: extend the existing collect/rewrite pipeline and shared sf-do boundary frontend instead of adding a transform/module/parser; keep one internal plan representation produced by transform and consumed/validated by runtime; generate only current Step functions plus mapped bridges; keep diagnostics at the transform owner with bounded Vsn/Block/Step/MFA metadata; use direct capability checks for router/export availability; preserve unexpected router/runtime exceptions and avoid broad catches; do not add a second manifest, historical executor, compatibility reader, state channel, callback API, or wrapper chain.
- Required evidence: current/historical analyzer reuse map; plan and bridge producer-to-consumer representation trace; router/plan normal-result versus exception table; generated/exported symbol inventory and collision proof; bounded searches showing no old-body generation, callback-store access, State offset, second manifest, version graph, or release mechanism; owned diff and validation exits.

## Ordered work

1. Extend frozen collection state to distinguish current sf-do plans from valid historical declaration input while retaining one lexical/qualifier frontend and aggregated diagnostics.
2. Validate router placement, literal declaration shape, explicit FrameOwner Vsn, unique exact source, historical/current named boundary lookup, raw Outcome normalization, and capture subset.
3. Allocate collision-safe bridge names, freeze the minimum source/target metadata, replace valid `sf_migrate/3` expressions with opaque plan values, auto-export the router, and emit raw bridges without historical bodies.
4. Extend current runtime resolution at the existing per-frame owner boundary: exact router/plan validation, mapped raw bridge precedence, then unchanged automatic fallback.
5. Perform a primary-path omission search over every new transform/runtime caller and remove any duplicate analyzer, manifest, representation conversion, broad exception inference, or callback/state ownership.

## Blocking self-validation

- Commands: `rebar3 compile`; `rebar3 xref`; `rebar3 eunit --module=async_sf_transform_tests`; `git diff --check`; bounded `rg` audits for `migrate_sf_callbacks|sf_migration_dependencies|release_handler|callbacks.*offset|historical.*manifest`

## Deferred downstream validation

- Commands: exact-Vsn/multiple-router/rename/raw-bridge/purge/unknown-source compiler and callback scenarios; full Common Test and supported OTP matrix
- Owner: task-test-1
- Expected transitional failures: no existing-suite failure is accepted; before task-test-1 the candidate intentionally lacks new document-driven sf-migrate fixtures and therefore cannot alone prove hot-code purge or callback-store non-mutation
- Closure gate: task-test-1 must cover task-coding-1 on its target-visible immutable candidate, add only test assets, pass focused/full gates, and receive independent test review; any product finding returns through a target-mediated fix task

## Observable completion

- A migration-enabled owner compiles complete exact historical raw mappings and rejects the required invalid forms with bounded diagnostics.
- A consumed old renamed Step can route through the current raw bridge while an unmapped compatible Step still uses automatic resolution.
- The production diff is confined to the two owned owners and contains no old workflow execution, callback mutation, second manifest, eager/down/release mechanism, or new dependency.

## Stop conditions

- Normative input or immutable base differs from this contract.
- Historical syntax or raw Outcome compatibility needs a new product decision not fixed by the specification.
- Callback storage/lifecycle must change, another production owner is required, or the shared analyzer cannot be reused without a parallel pipeline.
- A blocking baseline fails outside the owned change or overlapping unrelated edits cannot be preserved.

## Handoff

Return `Status`, exact candidate tree/diff identity, changed symbols, capability/representation/exception evidence, blocking command exits, omitted downstream evidence, and risks. Do not edit tests/docs/specs/workflow, stage/commit/merge, accept the task, provision lanes, or start task-coding-2.
