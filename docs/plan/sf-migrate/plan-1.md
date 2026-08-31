# sf-migrate generation-1 workflow plan

- Plan status: draft
- Goal: `sf-migrate`
- Generation: `1`
- Context: none
- Evidence target: `aa7c6cd4f6b79659b1293b753a7aa5dd2f98cf41`
- Target branch: `async-sf-cc`
- Normative inputs: `openspec/changes/sf-migrate/.openspec.yaml`, `changelog.md`, `proposal.md`, `design.md`, `implementation.md`, `specs/sf-migrate/spec.md`, `tasks.md`

## Material outcome

Extend the implemented sf-do/sf-cc boundary with the smallest one-way, reply-time source-to-current bridge. A current FrameOwner can declare one complete historical named block for an exact literal module Vsn, optionally convert the actual `(Outcome, Env)`, and continue through the current generated Step without scanning State, rewriting a pending callback/frame, replaying a request, or adding a downgrade phase.

Completion requires all of the following observable behavior:

1. `sf_do_code_change/2` remains ordinary Erlang routing; `sf_migrate/3` is accepted only on its return paths, is lowered to an opaque exact-source plan, and causes automatic export of the router.
2. Historical analysis reuses the current sf-do grammar/capture frontend, emits no historical workflow body, and rejects incomplete/unnamed input, missing targets, duplicate `{Vsn, Block}` declarations, generated collisions, and non-literal public shapes with bounded diagnostics.
3. Raw mappings compile only when normalized Outcome patterns are compatible and target captures are a subset of source captures; their generated version-specific bridge forwards the actual reply and Env directly to the current Step.
4. Optional external `convert/2` may return `{ok, NewOutcome, NewEnv}` or `{error, Reason}`. Explicit mappings precede automatic inheritance, target Outcome/Env are validated, and a matched-but-invalid bridge never silently falls back.
5. Designed incompatibility is sanitized and forwarded as `{error,{sf_runtime,{incompatible_frame,Info}}}` through the direct `parent_ref`; converter/router user exceptions preserve existing sf-cc user-code exception semantics.
6. Missing router, `unhandled`, or an unmapped Step falls back to existing named identity plus required-capture-subset resolution. Vsn mismatch alone is never failure.
7. Downgrade has no reverse converter or application phase: the actually loaded old manifest either resumes a compatible frame with the original Outcome and extra Env allowed, or fails only that callback at reply time.
8. Callback encoding/store, State layout, ordinary callbacks, unnamed sf-do same-version behavior, current manifest identity, linked cleanup, timers/monitors, and request lifecycle remain owned by the implemented sf-do/sf-cc boundary.

## Fact baseline

- Immutable HEAD and clean worktree were verified at `aa7c6cd4f6b79659b1293b753a7aa5dd2f98cf41`; the bounded normative and implementation paths have no delta from that commit.
- `async_sf_transform` already owns a frozen collect/rewrite pipeline, named `{1, Block, StepName}` identity, generated Step `/2`, literal module Vsn, minimal capture analysis, and the current-only `'$async_sf_manifest'/0`.
- `async_sf_runtime:continue/5` already validates one stored frame, loads its owner manifest at execution time, accepts target required captures as a SavedEnv subset, invokes the current generated MFA, and delegates structural failure to the direct parent.
- `async_sf_cc` already owns `#sf_callback{frame,acc_ref,parent_ref,timer_ref}`, node take, direct-parent success/failure dispatch, and lifecycle cleanup. `async_t` only routes each consumed stable action into `async_sf_runtime:continue/5`.
- No `sf_migrate`, `sf_do_code_change`, migration plan, release hook, dependency attribute, or old sf-migrate product implementation exists in current `src/`, `include/`, or `test/`.

## Scope and exclusions

In scope: exact historical declarations, shared historical boundary analysis, raw and converter bridges, reply-time explicit-plan precedence, automatic fallback, sanitized incompatibility, downgrade/no-side-effect behavior, public README documentation, and document-driven integration tests.

Excluded: StateHolder migration, callback-store enumeration or mutation, callbacks offsets, replacement frames, old workflow execution, old BEAM lookup, reverse converters, version graphs/multi-hop, app/module Vsn coercion or ordering, release-handler hooks, version registries, dependency graphs/order enforcement, ordinary callback migration, async runtime record-encoding migration, and any compatibility reader for superseded sf-migrate drafts.

## Capability disposition

| Capability | Decision | Semantic owner and generation-1 result |
|---|---|---|
| current sf-do grammar, lexical capture analysis, generated Step and manifest | reuse + extend | `async_sf_transform`; one shared frontend analyzes current and historical boundaries, while only current blocks generate workflow functions/manifest entries |
| exact declaration discovery and lowering | new in existing owner | `async_sf_transform`; no second transform or public plan representation |
| version-specific raw/converter bridge | new generated capability | `async_sf_transform`; internal `/2` bridge adapts one consumed old reply to one current Step |
| per-frame reply-time resolution | extend | `async_sf_runtime`; exact explicit mapping before the existing named automatic path |
| designed failure forwarding and linked lifecycle | reuse + extend | `async_sf_runtime` constructs bounded migration reasons; existing `structural_failure`/`async_sf_cc` route and clean the direct node |
| downgrade behavior | reuse | actual current manifest and existing automatic resolver; no down mechanism is added |
| callback/store/State APIs | reuse unchanged | `async_sf_cc`, `async_t`, `async_m`, and shared records remain authoritative |
| public surface documentation | extend | `README.md` and `README.zh.md` describe declarations, converter, fallback, failures, and downgrade boundary |
| acceptance coverage | new + reuse | one new sf-migrate CT suite/fixture plus unchanged existing EUnit/CT regression suites |

## Execution and merge route

All coding phases run serially in the fixed coding lane. Each later coding phase starts only from the preceding phase's accepted merge on target, so overlapping ownership in `async_sf_transform` and `async_sf_runtime` is sequential rather than parallel. `task-test-1` is a target-visibility barrier covering all three coding tasks and starts in integrate only after their accepted target merges are visible.

The only route is `coding -> target -> integrate -> target`. A product defect found by `task-test-1` is reported without editing production code; its correction receives a generated `task-test-1-fix-N` coding contract based on the observed target, returns through coding to target, then integrate refreshes from target and reruns the affected/full gates. Direct coding-to-integrate merges are forbidden.

## task-coding-1

OpenSpec Task 1 vertical slice: add exact historical declaration analysis, raw mapping validation, opaque plan/version-specific raw bridge generation, and runtime exact-plan precedence with automatic fallback. It owns only the transform/runtime seam and must leave callback storage and historical execution absent.

## task-coding-2

OpenSpec Task 2 vertical slice: on the accepted raw bridge, add external converter handling, generated target Outcome/Env validation, explicit-over-automatic semantics, and the sanitized designed-failure/user-exception boundary.

## task-coding-3

OpenSpec Task 3 production/documentation slice: close the direction-neutral downgrade boundary, prove by bounded omission audit that no eager/reverse/release mechanism exists, and publish the final English/Chinese surface. It may make a minimal runtime correction only within its owned seam; the expected production result is reuse, not a second down path.

## task-test-1

One hard, independent document-driven test phase covers all three accepted coding slices. It owns new sf-migrate fixtures/tests, validates real hot-code purge and A/B/A behavior, preserves existing baselines, and runs compile, EUnit, Common Test, xref, dialyzer, supported OTP matrix, strict OpenSpec validation, and residue/privacy audits on one immutable integrate candidate.

## Merge and completion gates

- Each coding candidate must pass its blocking checks, independent coding review, owned-path/diff audit, and merge to `async-sf-cc` before its successor base is established.
- `task-test-1` may start only after the three `Covers` tasks are accepted on target and integrate records that exact target commit as its base.
- No known product failure, unexplained warning, callback mutation, sensitive failure payload, direct coding-to-integrate merge, or unowned-path change may be accepted.
- Test changes merge to target only after the focused and full gates pass on the same candidate. Any product correction invalidates that evidence and requires the target-mediated fix/rerun loop.
- Initiative completion additionally requires target HEAD to contain all four accepted task results, a clean workflow validation, and no open deferred evidence. Only the dispatcher may accept the plan, provision lanes, update execution status/checklists, merge, or declare completion.
