# Coding Task Contract

- Task: task-coding-3
- Bundle generation: 1
- Phase artifact: task-coding-3.md
- Plan: plan-1.md
- Lane manifest: lanes-1.md
- Context: none
- Evidence target: task-coding-2 accepted merge on target branch async-sf-cc
- Task kind: coding
- Mode: normal
- Mode basis: prior tasks own the compiler/runtime mechanism; this slice only closes the direction-neutral downgrade/no-side-effect boundary and two public README surfaces
- Execution strategy: single-def
- Worker role: def_coding_worker
- Lane: coding
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-migrate-g1-coding
- Base: task-coding-2 accepted merge on target branch async-sf-cc
- Depends on: task-coding-2
- Covers: none

## Bounded goal

Complete OpenSpec Task 3's production/documentation portion without inventing a down mechanism: ensure the accepted runtime uses the actually loaded current manifest for both directions, compatible B-created frames resume on A with original Outcome/extra Env allowed, incompatible frames fail only at reply time, prohibited eager/reverse/release residue is absent, and the public English/Chinese documentation matches the final surface.

## Normative objectives

- Do not add an appdown entry point, reverse converter, callback scan/fail, candidate frame, target Vsn lookup, old code slot, release handler, version registry, dependency attribute/graph, or release-order enforcement.
- For B -> A, reuse exact current-manifest automatic named identity/capture-subset resolution; ignore extra saved Env and pass Outcome unchanged. Vsn terms have no ordering semantics.
- An incompatible down frame becomes the same bounded direct-parent reply-time incompatibility and does not block application downgrade or modify another node.
- Multi-owner linked nodes continue to resolve independently; ordinary and stable callback encodings, StateHolder `code_change/3`, and `async_m:fail_sf_callbacks/4` remain untouched.
- Document declaration/router placement, explicit Vsn, exact historical block, name-only mappings, raw compatibility, converter contract, explicit precedence, sanitized failures, automatic up/down inheritance, purge behavior, and troubleshooting. Clearly warn that converter business Reason may enter failure Outcome if retained.

## Owned paths

- `src/async_sf_runtime.erl` (only if a minimal correction is required to satisfy the specified direction-neutral reply-time behavior)
- `README.md`
- `README.zh.md`

`async_sf_transform`, sf-cc/async_m/async_t lifecycle owners, tests, OpenSpec, and workflow files are read-only. Absence verification may inspect them but must not create placeholder or cleanup changes.

## Preserved invariants

- task-coding-1/2 declaration, bridge, precedence, designed failure, and user-exception behavior remain unchanged.
- Existing public callback APIs and current manifest encoding remain additive-compatible for applications that do not use sf-migrate.
- Documentation describes only shipped behavior and exact failure/compatibility ceilings; deferred Rebar tooling is not presented as runtime capability.

## Implementation simplicity

- Standard: audit-implementation-simplicity
- Task-specific requirements: prefer zero new runtime code when the accepted resolver already satisfies downgrade semantics; any runtime edit must reuse the same current-manifest automatic path and canonical incompatibility result; add no direction graph, compatibility adapter, State hook, scanner, registry, private release query, or parallel documentation abstraction; keep English/Chinese sections structurally aligned and link to existing APIs rather than duplicating internals.
- Required evidence: B-frame -> loaded A manifest -> automatic current Step/failure primary-path trace; read-only caller search for appup/appdown and StateHolder boundaries; prohibited-symbol/responsibility no-match report with every retained `fail_sf_callbacks`/direction match explained; README surface-to-spec map; owned diff and validation exits.

## Ordered work

1. Trace the accepted runtime's Vsn mismatch, missing-router, automatic fallback, direct-parent failure, and directional callers against the actual current manifest.
2. Make only the minimal owned runtime correction if that trace violates a specified down observable; otherwise leave production code unchanged.
3. Audit the repository for eager scan, reverse plan/converter, callback offset migration, release/private-state/version/dependency residue and record bounded no-match/retained-match evidence.
4. Update both READMEs with one aligned public sf-migrate section and troubleshooting boundary; do not expose opaque plan/bridge function names.
5. Re-run compile/static checks and verify documentation/product symbols agree.

## Blocking self-validation

- Commands: `rebar3 compile`; `rebar3 xref`; `git diff --check`; bounded `rg` audits for `migrate_sf_callbacks|sf_migration_dependencies|release_handler|reverse.*convert|version.*registry|callback.*offset|target.*vsn`; README symbol/API consistency search

## Deferred downstream validation

- Commands: A->B->A, B-created compatible/incompatible frames, original down Outcome, no reverse converter, appdown no-side-effect, multi-owner/mixed callback, partial-release-window, README-linked behavior, and full repository validation
- Owner: task-test-1
- Expected transitional failures: no existing-suite failure is accepted; runtime hot-code/downgrade and no-side-effect claims remain unaccepted until task-test-1 executes real callback fixtures on the covered target candidate
- Closure gate: task-test-1 must cover task-coding-3, prove all downgrade and integration observables, pass full gates, and send any product correction through coding -> target before evidence is rerun

## Observable completion

- Down behavior is entirely reply-time/current-manifest based, with no new application phase or callback mutation; compatible/incompatible semantics are ready for independent black-box proof.
- English and Chinese READMEs expose the same bounded declaration/converter/fallback/failure/down contract.
- Prohibited eager/reverse/version/dependency mechanisms are absent, and any runtime diff is the minimum correction in the existing owner.

## Stop conditions

- task-coding-2 is not accepted on target or leaves unresolved product behavior.
- Correct downgrade behavior requires editing StateHolder/sf-cc/async_t/async_m or defining a new release policy.
- English/Chinese public behavior cannot be derived from the current normative specification.
- Blocking checks fail outside owned paths or overlapping unrelated edits cannot be preserved.

## Handoff

Return `Status`, exact candidate, runtime trace and whether code changed, README/spec map, prohibited-residue audit, command exits, deferred evidence, and risks. Do not edit tests/specs/workflow, stage/commit/merge, accept the phase, or start task-test-1.
