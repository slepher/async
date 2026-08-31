# sf-migrate generation-2 planning context

- Goal: `sf-migrate` test enhancement
- Generation: `2`
- Repository: `/home/slepher/project/erl_projects/async-sf-cc`
- Target branch: `async-sf-cc`
- Immutable target HEAD: `a980e9086a7e142c888d82f75e94d49f55334f51`
- Worktree state at collection: clean
- Collected: `2026-08-31`

## Persistent user decisions

- Generation-1 is complete and remains historical evidence. Do not overwrite or extend `plan-1.md`, `lanes-1.md`, or its task/review artifacts.
- Generation-2 is test-only. Production, README, OpenSpec, existing unrelated tests, configuration, and workflow generations other than generation-2 are outside implementation authority.
- The realistic fixture has two FrameOwner modules: an `order_flow` parent `sf_do` embeds a `payment_flow` child `sf_do`. Each owner has exact literal module versions A, B, and C.
- Required upgrade observations include A->B->C, A->C, parent and child callbacks handled by different owner versions, and mixed rollout with order at C while payment remains at B.
- A callback that remains pending across two upgrades must resolve in C directly from A using C's explicit A->C declaration. It must not infer or execute A->B->C. C must explicitly retain both A->C and B->C declarations for every changed supported source boundary.
- Resolution remains reply-time and one-shot. Tests must cover nested Outcome propagation, old-code purge, late reply, direct `parent_ref`, converter and raw paths, privacy/failure, unfinished nested callbacks, and representative multi-owner/version combinations.
- Existing regression authority must be reused rather than duplicated. Tests may add assertions for lifecycle behavior only when a real nested cross-version boundary makes that behavior observable.
- The parent test profile is `strict`; a reviewed immutable checkpoint is required before read-only `full_tester` verification.
- The only correction route is `coding -> target -> integrate -> target`. A test worker reports product findings and never edits production.

## Current implementation facts

- `async_sf_transform` analyzes each named boundary independently, freezes exact literal module Vsn, `{1, Block, StepName}` identity, generated Step MFA, Outcome shape, and required capture names. Historical declarations generate metadata and a version-specific bridge, not an executable historical workflow.
- `async_sf_runtime:continue/5` resolves one consumed frame against that frame's currently loaded owner manifest. On Vsn mismatch it asks the same owner for an exact `{StoredVsn, StoredBlock}` plan, prefers an explicitly mapped Step, then falls back to named identity plus required-capture subset compatibility.
- Runtime does not compose version paths. The selected current manifest's plan must directly name the stored source Vsn and current target Vsn.
- `async_sf_cc` owns node take, direct-parent success/failure dispatch, one-shot cleanup, late-reply behavior, and nested continuation linkage. Migration code does not enumerate or rewrite the callback store.
- `async_t` records the frame and its direct `parent_ref` when a stable action is materialized. A child `sf_do` can therefore leave a child action plus an owner-specific parent continuation to be resolved independently when the child Outcome returns.
- Designed migration failures are bounded `{error,{sf_runtime,{incompatible_frame,Info}}}` outcomes. `Info` contains only module, stored/current Vsn, identity, and normalized reason; converter/router exceptions retain native user-code exception behavior.

## Public contract facts

- README and OpenSpec define one-way source-to-current bridges, exact literal Vsn matching, explicit-over-automatic precedence, raw compatibility only for equal normalized Outcome shape plus target-capture subset, and optional external `convert/2` for Outcome/Env conversion.
- Downgrade and rollout ordering add no eager scan, reverse converter, target discovery, dependency graph, or release gate. A callback observes whichever owner version is actually loaded at reply-time.
- A current C owner that supports pending A and B frames must carry separate exact declarations. Historical declarations are not predecessors and are never chained.
- Failure is forwarded to the current node's direct parent; successful child completion resumes the parent using the child's actual Outcome.

## Existing test evidence and generation-2 delta

- `test/async_sf_migrate_SUITE.erl` already covers A/B declaration diagnostics, raw bridge with purge, converter precedence and validation, sanitized failure, fallback, A/B/A downgrade, B-created downgrade, late reply/one-shot behavior, and two parallel owners.
- Existing sf-do/sf-cc suites remain authority for ordinary callbacks, base linked continuation behavior, timer/monitor cleanup, callback summaries, and current manifest/store encoding.
- Missing evidence is a genuinely nested parent-child graph across two owners and three versions, direct A->C selection after an unobserved intermediate B, mixed owner rollout, child Outcome re-entry into a differently versioned parent, unfinished nested callback behavior, and a bounded representative owner/version matrix.
- Generation-2 therefore adds no compiler-only matrix already covered by generation-1 and no production-facing assertion against opaque plan or generated bridge names.

## Planning topology decision

- Use one `normal`, `split-def` test parent with two flat, serial subtasks in the integrate lane.
- `task-test-2.1` owns a reusable dynamic nested fixture and the canonical nested flow suite.
- `task-test-2.2` owns a separate representative matrix suite and reads the accepted fixture from `task-test-2.1` without editing it.
- The split is justified by distinct observable boundaries and disjoint review ownership: canonical nested lifecycle/fixture correctness versus additional version/owner/failure combinations. A third subtask or Cartesian product is not required.
