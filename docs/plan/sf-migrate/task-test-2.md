# Test Task Contract

- Task: task-test-2
- Bundle generation: 2
- Phase artifact: task-test-2.md
- Plan: plan-2.md
- Lane manifest: lanes-2.md
- Context: context-2.md
- Evidence target: a980e9086a7e142c888d82f75e94d49f55334f51
- Task kind: test
- Mode: normal
- Mode basis: canonical nested fixture/flow coverage and the representative matrix have separate owned paths and review boundaries, while one strict parent gate must validate their cumulative candidate
- Execution strategy: split-def
- Worker role: def_coding_worker
- Parent verification role: full_tester
- Test profile: strict
- Lane: integrate
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-migrate-g2-integrate
- Base: a37d5335101c0d24169468f94614f3f8f7361e3d
- Depends on: none
- Covers: none

## Bounded goal

Add only public black-box Common Test evidence for a genuinely nested `order_flow` parent and `payment_flow` child across two FrameOwners and exact A/B/C versions. Preserve current reply-time, exact-literal, direct-parent, one-shot, converter/raw, failure/privacy, and no-multi-hop semantics. Do not modify or standardize production internals to make the fixture convenient.

## Required observable coverage

1. Two owners and real nesting: the order owner enters a payment owner `sf_do`; callback summaries and final outcomes distinguish each owner and show child-to-parent continuation.
2. Three versions: exercise A->B->C and A->C with exact literal Vsns. C source must visibly declare direct A->C and B->C mappings for each changed supported boundary.
3. Direct, not chained: an A-created pending callback survives loading B and then C without reply. On reply at C, only A->C converter/bridge counters change; A->B, B->C-for-B-source, historical body, and request counters stay untouched except for the originally issued request.
4. Different reply-time versions: parent and child callbacks resume under different current owner versions, including order C/payment B mixed rollout and a later independent owner upgrade.
5. Nested Outcome: the child's actual converted or raw Outcome becomes the input consumed by the current parent continuation and appears in the final outer result exactly once.
6. Lifecycle: purge superseded owner code, retain unfinished nested callbacks until reply, preserve direct `parent_ref`, consume each node once, return late reply as unhandled, and produce no duplicate outer completion.
7. Compatibility: cover parent-converter/child-raw and parent-raw/child-converter, plus an automatic-compatible unchanged nested step omitted from an explicit historical plan.
8. Failure/privacy: nested converter rejection, invalid target value, or incompatible source produces the bounded current-node failure, follows direct parent failure routing, consumes no sibling callback, and leaks no Env/Outcome/State/callback/fun/sensitive sentinel.
9. Representative matrix: cover multiple owner/version pairings and incomplete nested callback timing without enumerating every A/B/C Cartesian combination.

## Existing coverage to reuse

- `test/async_sf_migrate_SUITE.erl` remains authority for single-level declaration diagnostics, exact literal mismatch, raw purge, converter target validation, fallback, privacy, A/B/A and B-created downgrade, late reply, and non-nested multi-owner behavior.
- Existing sf-do/sf-cc suites remain authority for ordinary callback behavior, generic linked cleanup, timer/monitor behavior, callback-store encoding, and manifest identity.
- Generation-2 cases must include either a real nested owner boundary or a direct three-version A->C observation. Do not copy generation-1 cases merely to rename modules or versions.

## Parent owned paths

- `test/async_sf_migrate_nested_fixture.erl`
- `test/async_sf_migrate_nested_SUITE.erl`
- `test/async_sf_migrate_nested_matrix_SUITE.erl`

All `src/`, `include/`, README, OpenSpec, workflow/status files, existing test suites, build configuration, and unrelated fixtures are read-only to task workers. A product defect is a finding, not authority to edit production.

## Test design constraints

- Use dynamic source compilation/loading and existing public `async_m`/callback inspection seams. Do not assert opaque plan maps, generated private bridge atoms, AST digests, or callback record internals.
- Fixture versions use exact literal A/B/C terms and explicit declaration text. C must carry both A and B source declarations; no helper may synthesize a predecessor chain.
- Counters/sentinels must distinguish original request issuance, each explicit converter, current Step execution, historical body non-execution, child completion, parent resumption, and outer completion.
- Keep the matrix pairwise and requirement-driven. Add a combination only when it proves a distinct owner, source/current Vsn, raw/converter, failure, or timing boundary.
- Freeze exact target/checkpoint identity before every run. Any product correction or candidate change invalidates downstream evidence.
- Test code is judged for observable fidelity, bounded ownership, and maintainable use of existing test capabilities; task-test work does not apply or perform an `audit-implementation-simplicity` implementation review.
- Do not weaken existing assertions, mark product failures expected, add sleeps for ordering, or edit production/docs/specs to make a test pass.

## Subtask task-test-2.1

- Goal: create the shared dynamic A/B/C two-owner fixture and prove the canonical nested lifecycle, direct A->C selection, mixed rollout, Outcome propagation, purge, one-shot, and late-reply path
- Worker role: fresh `def_coding_worker`
- Lane: integrate
- Exact target base: a37d5335101c0d24169468f94614f3f8f7361e3d
- Starting phase base: integrate HEAD must equal a37d5335101c0d24169468f94614f3f8f7361e3d
- Depends on sibling subtasks: none
- Covers: parent objectives 1-6 and the canonical instances of 7-9
- Owned paths: `test/async_sf_migrate_nested_fixture.erl`, `test/async_sf_migrate_nested_SUITE.erl`
- Excluded paths: `test/async_sf_migrate_nested_matrix_SUITE.erl`, existing tests, product, docs/specs, configuration, and workflow/status artifacts
- Product authority: none; report exact product evidence and stop

### Subtask 2.1 required work

1. Verify clean integrate identity and exact base; run the existing sf-migrate suite before writing.
2. Build only the fixture source/configuration needed for two owner module names, order/payment A/B/C versions, explicit exact migration declarations, counters, and sensitive sentinels.
3. Make the order parent issue/await its own boundary, enter the payment child, and retain a parent continuation while the child reply is pending.
4. Add a canonical trace that leaves an A callback pending while B then C are loaded, proves C chooses direct A->C, and proves C source independently retains B->C.
5. Add the order C/payment B mixed rollout trace, then complete the child and verify its Outcome resumes the correct current order parent exactly once.
6. Assert owner-specific summaries/parent references before and after replies, old-code purge, request/old-body/converter/current-Step counters, no eager mutation, empty completion state, late-reply unhandled, and no duplicate completion.
7. Return exact fixture version map, scenario-to-case matrix, changed paths, commands/exits, and any product/environment finding.

### Subtask 2.1 focused validation

- `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl`
- `rebar3 ct --suite=test/async_sf_migrate_nested_SUITE.erl`
- `git diff --check`

Every added case must execute in these commands. Focused success is not parent acceptance.

### Subtask 2.1 full validation

- Full validation: deferred unchanged to the `task-test-2` strict `full_tester` gate in `Blocking self-validation`; subtask 2.1 must remain byte-identical in the final cumulative checkpoint, and any later edit requires its focused run and checkpoint review to repeat before the full gate

### Subtask 2.1 deferred evidence owner

- Owner: `task-test-2` parent `full_tester`
- Deferred evidence: cumulative generation-2 matrix, complete EUnit/CT/xref/dialyzer/docker/OpenSpec gates, and prohibited/private-payload audits
- Closure: dispatcher creates a reviewed immutable `task-test-2.1` checkpoint; after `task-test-2.2`, parent `full_tester` reruns all deferred commands on the final cumulative checkpoint with zero unexplained failures or skips

### Subtask 2.1 completion and handoff

- Complete when both owned paths alone implement the canonical objective, focused commands pass on unchanged content, owned-path review passes, and dispatcher records the immutable checkpoint.
- Handoff returns `Status`, exact base/start/checkpoint, cases, fixture version/declaration map, diff paths, focused commands/exits, counters/privacy evidence, findings, and remaining parent-owned evidence.
- Stop on base drift, owned-path conflict, private-representation dependency, product failure, privacy leak, inferred chaining, request replay, duplicate completion, or need to edit any excluded path.

## Subtask task-test-2.2

- Goal: add the disjoint representative nested owner/version, raw/converter, failure/privacy, and unfinished-callback matrix without repeating canonical or generation-1 cases
- Worker role: fresh `def_coding_worker`
- Lane: integrate
- Exact target base: a37d5335101c0d24169468f94614f3f8f7361e3d
- Starting phase base: dispatcher must bind the reviewed immutable `task-test-2.1` checkpoint to its full exact commit before dispatch; that commit's target ancestor must be a37d5335101c0d24169468f94614f3f8f7361e3d, and a symbolic-only base is invalid
- Depends on sibling subtasks: task-test-2.1 checkpoint and review
- Covers: parent objectives 7-9 and nonduplicated variants of 2-6
- Owned paths: `test/async_sf_migrate_nested_matrix_SUITE.erl`
- Read-only dependency: `test/async_sf_migrate_nested_fixture.erl`
- Excluded paths: `test/async_sf_migrate_nested_SUITE.erl`, fixture edits, existing tests, product, docs/specs, configuration, and workflow/status artifacts
- Product authority: none; report exact product evidence and stop

### Subtask 2.2 required work

1. Verify the reviewed `task-test-2.1` checkpoint, exact target ancestry, clean worktree, fixture content, and canonical focused evidence before writing.
2. Add a compact pairwise matrix containing at least: parent converter/child raw; parent raw/child converter; order A->C with payment A->B->C; order A->B->C with payment A->C; and order C/payment B while a nested callback remains unfinished.
3. Include one automatic-compatible nested step omitted from an explicit plan and prove exact fallback without a no-op mapping.
4. Include one nested designed failure/privacy case and verify direct-parent routing, sibling isolation, bounded Info keys/reason, sentinel absence, one-shot cleanup, and no fallback after a selected invalid explicit bridge.
5. Assert C's explicit A->C and B->C behavior through public counters/outcomes and fixture declarations, never through private plan or bridge names.
6. Run its suite plus both existing/canonical migration suites on the unchanged cumulative candidate; only test-owned harness/case defects may be corrected.
7. Return a matrix showing why each combination is distinct and which existing regression owns every intentionally omitted case.

### Subtask 2.2 focused validation

- `rebar3 ct --suite=test/async_sf_migrate_nested_matrix_SUITE.erl`
- `rebar3 ct --suite=test/async_sf_migrate_nested_SUITE.erl,test/async_sf_migrate_SUITE.erl`
- `git diff --check`

Every added matrix case must execute. Focused success is not parent acceptance.

### Subtask 2.2 full validation

- Full validation: the `task-test-2` strict `full_tester` gate in `Blocking self-validation` runs on the reviewed cumulative subtask 2.2 checkpoint; this subtask supplies no substitute or reduced full command set

### Subtask 2.2 deferred evidence owner

- Owner: `task-test-2` parent `full_tester`
- Deferred evidence: complete repository/OTP/OpenSpec validation and cumulative privacy/prohibited-mechanism audit
- Closure: dispatcher creates and reviews the immutable cumulative checkpoint, then `full_tester` runs every parent blocking command on that exact commit; any candidate change requires a new checkpoint and rerun

### Subtask 2.2 completion and handoff

- Complete when its sole owned path closes the nonduplicated matrix, all focused commands pass on the cumulative candidate, owned-path review passes, and dispatcher records the immutable cumulative checkpoint.
- Handoff returns `Status`, exact target ancestor/start/checkpoint, matrix and omission rationale, diff path, focused commands/exits, failure/privacy evidence, findings, and parent-gate readiness.
- Stop on checkpoint drift, fixture change requirement, duplicate canonical/generation-1 coverage, product failure, leak, swallowed exception, inferred chain, unowned edit, or unavailable required focused command.

## Subtask checkpoint and review gate

- The dispatcher, not workers, creates immutable checkpoints; workers never stage or commit.
- Each checkpoint receives a lightweight test review for objective coverage, observable assertions, disjoint ownership, failure ownership, and focused evidence. It does not perform implementation-simplicity review.
- `task-test-2.2` starts only after `task-test-2.1` checkpoint review passes. Parent verification starts only after the cumulative checkpoint review passes.
- A changed candidate invalidates the affected review and every downstream run.

## Parent full_tester verification

- Task ID: task-test-2
- Generation: 2
- Source branch: async-sf-cc
- Test branch: codex/sf-migrate-g2-integrate
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Target base: a37d5335101c0d24169468f94614f3f8f7361e3d
- Candidate: dispatcher must bind the reviewed immutable cumulative `task-test-2.2` checkpoint to its full exact commit before `full_tester` dispatch; a symbolic-only candidate is invalid
- Test profile: strict
- Timeout: 60 minutes per command
- Mutation authority: none

`full_tester` first verifies branch, HEAD, target ancestry, worktree cleanliness, reviewed checkpoint identity, and exact changed paths. It then runs all blocking commands read-only on that one candidate and returns exact commands, exits, environment, output/artifact locations, skips, and unavailable evidence.

## Blocking self-validation

- Commands: `rebar3 compile`; `rebar3 eunit`; `rebar3 ct`; `rebar3 xref`; `rebar3 dialyzer`; `rebar3 docker_ci`; `npx openspec validate sf-migrate --strict`; `git diff --check`; bounded searches for prohibited eager/reverse/version-graph mechanisms and for sensitive payload/private bridge assertions in generation-2 tests

Every command is blocking. An environment-unavailable result is not a pass and must use the evidence ownership below.

## Deferred evidence ownership

- Owner: dispatcher retains closure; `full_tester` produces the evidence
- Accepted temporary condition: a named command may be environment-unavailable only with exact command, exit, tool/runtime version, and failure output; product/test failure is never classified as unavailable
- Closure gate: rerun the exact command on the same immutable candidate in an authorized matching environment, or create a new reviewed checkpoint and rerun the entire affected/full gate; no open item permits merge readiness or completion

## Required parent evidence

- Exact target base, subtask starting commits, reviewed checkpoints, final candidate/tree, changed paths, and command lines/exits/environment.
- Fixture map for both owner names and A/B/C literal Vsns; explicit C A->C and B->C declaration evidence without private plan assertions.
- Canonical event/counter trace proving direct A->C, no intermediate execution, original request count, purge, child Outcome propagation, direct parent continuity, one-shot completion, and late reply.
- Pairwise matrix with requirement rationale and explicit reuse mapping to generation-1/existing suites for omitted regressions.
- Failure/privacy evidence with exact bounded Info keys/reason, sentinel search, direct-parent/sibling behavior, and cleanup.
- Full repository/OTP/OpenSpec reports plus prohibited-mechanism/private-assertion audit and closure for every unavailable item.

## Observable completion

- All generation-2 cases pass on one immutable cumulative candidate and demonstrate real nested cross-owner A/B/C behavior through public outcomes, summaries, counters, code load/purge, and callback handling.
- Direct A->C is distinguished from chaining; C retains separate A and B declarations; mixed rollout and child-to-parent re-entry are proven; no duplicate single-level regression or private implementation contract is introduced.
- Existing suites pass unchanged, changed paths are exactly the three parent-owned test files, no product finding/open evidence remains, and the reviewed candidate reaches target through the dispatcher.

## Merge and correction gate

- Test changes are merge-ready only after both focused reviews and parent full validation pass on the same cumulative candidate lineage.
- Product finding: stop with exact reproduction. Dispatcher issues `task-test-2-fix-N` in coding from the observed target; correction follows `coding -> target -> integrate -> target`, followed by integrate refresh and repeated focused/review/full gates.
- No direct coding-to-integrate merge, test-side production fix, accepted failure, hidden skip, or stale evidence is allowed.

## Parent handoff

Return `Status`, `Result`, exact base/checkpoints/candidate, coverage and reuse matrices, changed paths, focused/full commands and exits, privacy/direct-chain evidence, findings, deferred evidence with owner/closure, and merge-readiness recommendation. Do not edit product/docs/specs/workflow/status, stage/commit/merge, or declare initiative complete.
