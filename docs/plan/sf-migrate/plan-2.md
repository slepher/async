# sf-migrate generation-2 test enhancement plan

- Plan status: accepted
- Goal: `sf-migrate`
- Generation: `2`
- Context: context-2.md
- Evidence target: `a980e9086a7e142c888d82f75e94d49f55334f51`
- Target branch: `async-sf-cc`
- Normative inputs: current `src/async_sf_transform.erl`, `src/async_sf_runtime.erl`, `src/async_sf_cc.erl`, `src/async_t.erl`, `README.md`, `README.zh.md`, `openspec/changes/sf-migrate/**`, and `test/async_sf_migrate_SUITE.erl`

## Material outcome

Add the smallest complete public black-box evidence that sf-migrate preserves a realistic nested order/payment continuation graph across two independent FrameOwners and exact A/B/C module versions, without changing production semantics or duplicating generation-1 regression coverage.

Completion requires observable proof that:

1. `order_flow` is a parent named `sf_do` that enters a `payment_flow` child named `sf_do`; each pending frame resolves only against its own currently loaded FrameOwner.
2. Canonical A->B->C and A->C paths work at reply-time, including parent and child callbacks handled under different owner versions and order C/payment B mixed rollout.
3. An A-created frame held across both upgrades invokes C's exact A->C bridge directly. C contains separate explicit A->C and B->C declarations, and counters prove no A->B bridge, B historical body, request replay, or inferred multi-hop executes.
4. A child Outcome resumes the direct parent continuation and reaches the final outer completion exactly once, including when the child and parent resume under different owner versions.
5. Old owner code can be purged after the current declarations are loaded; the original request is not reissued, unfinished nested callbacks are not eagerly rewritten or failed, late replies are unhandled, and callback summaries/parent references show one-shot direct linkage.
6. Representative raw/converter combinations work in both parent/child positions. Explicit converter failure, invalid result, and incompatible raw fallback remain bounded, private, and scoped to the nested direct-parent chain.
7. A bounded matrix covers multiple owner/version combinations without a Cartesian explosion. Existing generation-1 and sf-do/sf-cc suites remain the authority for non-nested compiler, downgrade, ordinary callback, and generic lifecycle regressions.

## Scope and exclusions

In scope: test-owned dynamic A/B/C owner sources; one canonical nested flow suite; one representative matrix suite; focused Common Test runs; exact target/checkpoint evidence; and the strict immutable full-suite gate.

Excluded: every `src/` or `include/` change; README/OpenSpec edits; new product API; private plan/bridge-name assertions; eager migration; version graph or multi-hop logic; exhaustive owner/version Cartesian products; duplicate single-level compiler/converter/downgrade cases; test weakening; and any product repair performed by a test worker.

## Test capability disposition

| Capability | Decision | Generation-2 owner |
|---|---|---|
| dynamic compile/load/purge and callback inspection harness | reuse + narrow extension | reuse generation-1 public test patterns; place shared A/B/C nested source construction in `test/async_sf_migrate_nested_fixture.erl` |
| canonical nested lifecycle | new test-owned coverage | `test/async_sf_migrate_nested_SUITE.erl` |
| representative owner/version and raw/converter/failure matrix | new test-owned coverage | `test/async_sf_migrate_nested_matrix_SUITE.erl` |
| single-level sf-migrate acceptance | reuse unchanged | `test/async_sf_migrate_SUITE.erl` |
| ordinary sf-do/sf-cc lifecycle and encoding regression | reuse unchanged | existing repository EUnit/CT suites and parent full gate |
| production migration behavior | reuse unchanged | current implementation; findings route to a later coding correction contract |

## Execution topology

Use one `normal`, `split-def` parent in the fixed integrate lane. `task-test-2.1` first establishes the shared fixture and canonical nested observations. After its focused run and reviewed immutable checkpoint, `task-test-2.2` adds only the disjoint matrix suite and reads the fixture without modifying it. This is the minimum split that gives each worker an independently reviewable observable boundary and freezes non-overlapping owned paths.

No standalone `task-test-2.1.md` or `task-test-2.2.md` is created; both contracts are embedded in `task-test-2.md`.

## Execution and correction route

The planning target is `a980e9086a7e142c888d82f75e94d49f55334f51`; after the dispatcher commits this accepted planning bundle, the execution base is the resulting target `a37d5335101c0d24169468f94614f3f8f7361e3d`. Test-owned work is checkpointed in integrate and reaches target only after focused reviews and the strict parent gate.

Any product finding stops the test task. The dispatcher creates `task-test-2-fix-N` in coding from the observed target; the only route is `coding -> target -> integrate -> target`. Integrate must refresh from the corrected target and repeat affected focused checks, checkpoint review, and the complete parent gate. Direct coding-to-integrate merge and test-side product repair are forbidden.

## task-test-2

One target-visible test parent adds the nested A/B/C fixture and two disjoint suites through embedded `task-test-2.1` and `task-test-2.2` contracts. It then binds a reviewed immutable cumulative checkpoint to `full_tester` under the strict profile. The parent owns no production behavior and `Covers: none`; its visibility barrier is the exact target base and any later correction reaching target before integrate refresh.

## Acceptance and completion gates

- Each subtask verifies its exact starting lineage, edits only its owned paths, runs every changed case, returns a scenario-to-case matrix, and passes checkpoint review before the next subtask or parent gate.
- Cumulative focused validation must include both generation-2 suites plus the existing sf-migrate suite.
- Parent `full_tester` runs the complete repository/OpenSpec/OTP gate read-only on one immutable cumulative checkpoint. Candidate change invalidates all parent evidence.
- No unexplained skip, flaky result, privacy leak, duplicate completion, old-body/request replay, inferred chain, unowned edit, product finding, or open deferred evidence may be accepted.
- Completion is observable only when test assets are on target, the target contains the reviewed full-gate candidate, workflow validation passes, and the dispatcher closes every evidence item. Planner and workers do not accept, stage, commit, merge, or declare initiative completion.
