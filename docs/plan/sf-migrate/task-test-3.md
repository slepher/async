# Test Task Contract

- Task: task-test-3
- Bundle generation: 3
- Phase artifact: task-test-3.md
- Plan: plan-3.md
- Lane manifest: lanes-3.md
- Context: context-3.md
- Evidence target: 7552a0c7d6bd1cd167b9822534ad7327352e1f81
- Task kind: test
- Mode: hard
- Mode basis: fixture compilation, release generation/install, node lifecycle, controller identity, and all scenario assertions form one continuous cross-version state machine
- Execution strategy: single-sup
- Worker role: sup_coding_worker
- Parent verification role: full_tester
- Test profile: strict
- Lane: integrate
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-migrate-g3-integrate
- Base: 7552a0c7d6bd1cd167b9822534ad7327352e1f81
- Depends on: none
- Covers: none

## Bounded goal

Implement `test.md` as real public black-box application and release evidence. Build static same-name A/B/C `sf_fixture` applications, keep the held-reply controller outside their trees, execute isolated-node and real release-upgrade scenarios, and verify every required T/N matrix row without changing product behavior or existing evidence.

## Owned paths

- `test/app_fixtures/**`
- `test/test_async_controller.erl`
- `test/async_sf_migrate_application_SUITE.erl`

All `src/`, `include/`, existing test files/fixtures, README/OpenSpec, `rebar.config*`, workflows, `docs/plan/**`, and `test.md` are read-only. Generated release files belong under Common Test private data or `_build/test`; never write generated artifacts into owned source directories.

## Required fixture and controller work

1. Create A/B/C directories with the same `sf_fixture` application name and the same module names `sf_fixture_app`, `sf_fixture_sup`, `order_flow`, and `payment_flow`. Each version has distinct `.app` vsn/observable markers and version-correct `.appup`; do not duplicate modules that can own the required supervisor and worker callback clauses coherently.
2. Expose a complete registered tree `sf_fixture_sup -> order_sup -> order_worker` and `sf_fixture_sup -> payment_sup -> payment_worker`. Public fixture APIs/messages report application version, phase, worker owner, completion counts, counters, and bounded errors; tests do not inspect records, generated bridge names, plans, AST data, or callback internals.
3. Implement `test_async_controller` as a standalone `gen_server` started by the suite. Store exact `{Pid,MRef}` values received from `async_gen_server:call/2`, emit `{controller_held,MRef,Request}`, reply only through `gen_server:reply/2`, delete live entries once, retain replied identity sufficiently to distinguish `{error,already_replied}` from `{error,unknown_mref}`, and expose only the pending/count/identity observations required by the specification.
4. Fixture workers use only `async_gen_server:call(Controller,Request)` and `async:promise_mref(MRef)`. Order and payment create independently identifiable parent/child MRefs, summaries, and completion markers; a child result re-enters only its stored direct parent.

## Required node and release harness

1. Start three temporary distributed nodes with isolated code paths, one fixture version each, plus separate controllers. Verify application lists, application/supervisor/worker pids, registered tree structure, version markers, and cross-node/controller independence. Synchronize with held events, RPC results, monitors, and explicit replies; no sleeps.
2. Stage compiled A/B/C fixture applications and current immutable async/dependency artifacts under suite-private release roots. Generate runtime-correct A/B/C `.rel`, script/boot, relup, and release tar artifacts using public OTP `systools`; keep one release name and distinct release versions.
3. Start a separate A release node, then perform A->B->C and fresh A->C through `release_handler:unpack_release`, `check_install_release`, and `install_release` (or the exact public equivalents required by the installed OTP). Assert `which_releases` and application/tree/version transitions. Code-path changes and `code:load_binary/3` may prepare neither an upgrade nor its proof.
4. Preserve the controller pid, controller state, outstanding MRefs, application availability, and tree queryability across each install. Purge assertions must follow only what the generated release instructions and OTP tooling permit.
5. Capture OTP/ERTS/rebar versions, every generation/install call and result, fixture metadata and generated artifact paths, node command/log/status, release transitions, and scenario observations for review/full-test evidence.

## Scenario coverage

- Implement every `test.md` row T01-T18 and N01-N10. Keep the identifiers visible in Common Test case names, groups, or a one-to-one scenario table so omissions and failures are attributable.
- T01-T03 are no-upgrade baselines for A/B/C. T04-T07 distinguish A->B, B->C, direct C handling after A->B->C, and fresh A->C; direct cases prove B bridge/converter/body counters do not run.
- T08-T10 prove reply before, between, and after upgrades. T11-T18 prove multiple source versions, multiple same-source MRefs, arbitrary order, duplicate/unknown reply semantics, controller/tree continuity, concurrent old/new calls, and each frame's stored version ownership.
- N01-N10 prove independent parent/child ownership, mixed order/payment rollout, direct A->C, raw/converter combinations, out-of-order parent/child replies, bounded child failure, sibling isolation, no Env/internal leakage, late/duplicate child handling, and strictly one outer completion.
- Assert each pre-start/start/held/upgrade/reply/failure checkpoint listed in `test.md`. Original request count is one; no historical body/converter runs early; only the selected reply-time route/current step increments; controller pending and callback/completion state close exactly once.

## Reuse and simplicity constraints

- Reuse existing public async APIs, Common Test/EUnit assertion macros, OTP application/supervisor/RPC/release APIs, and accepted generation-2 outcome/counter vocabulary. Test task review judges observable correctness, ownership, and evidence; it does not apply the implementation-simplicity audit to test code.
- Use OTP `systools` and `release_handler` directly; add no dependency, repository release configuration, generic framework, private release-state reader, polling sleep, or production-only test seam.
- Keep release/node helpers inside the one suite unless a separately compiled controller or the mandated static fixture module must exist. Generate runtime-dependent `.rel` files rather than checking in OTP-version-specific copies.
- Do not weaken or rewrite existing tests. Do not classify a product failure as a skip, change expected behavior to match the implementation, or repair product code.

## Focused self-run

- `rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`
- `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl,test/async_sf_migrate_nested_SUITE.erl`
- `git diff --check`

The worker must run the suite it adds, report exact commands/exits/case counts/skips/log paths, and correct only owned test fixture/harness/assertion defects. A product failure stops with a minimal public reproduction and exact release/node evidence.

## Blocking self-validation

- Commands: `rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`; `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl,test/async_sf_migrate_nested_SUITE.erl`; `rebar3 ct`; `rebar3 eunit`; `rebar3 xref`; `rebar3 dialyzer`; `git diff --check`
- Binding: all commands run on the reviewed immutable cumulative candidate after exact branch/HEAD/target-ancestry/cleanliness/changed-path checks
- Failure rule: any available-command failure, assertion failure, hidden skip, fake upgrade path, unowned change, private-data dependency, leaked value, or unexplained scenario omission blocks merge readiness

## Environment-dependent verification

- Commands: real release cases inside the application suite; optional `rebar3 docker_ci` OTP 21-29 matrix when the required container runtime/images are available
- Owner: `full_tester` produces evidence; dispatcher retains disclosure and rerun closure
- Unavailable classification: only a missing/incompatible external runtime, release prerequisite, container service/image, or OS facility, with exact command/call, OTP/rebar/runtime versions, status/exit, output, node log, and last generated artifact
- Not unavailable: compile error, bad fixture metadata/appup/relup, release install rejection caused by the candidate, assertion failure, timeout after a started scenario, product failure, skipped case without exact prerequisite evidence, or use of an alternate path
- Closure gate: rerun the exact release case or matrix command on the same immutable candidate in a matching authorized environment; if the candidate changes, repeat parent review and all affected gates
- Reporting: unavailable release evidence is `unverified`, never `passed`; independent-node and non-release cases still execute and report separately

## Required evidence

- Exact target base, integrate start, reviewed candidate/tree, changed paths, tool/runtime versions, commands, exits, case counts, skips, and CT log/artifact paths.
- A/B/C application/module/version table; `.app`/`.appup` inputs; generated `.rel`/boot/relup/tar paths and hashes; public `release_handler` transitions/results; node commands/logs/status.
- Public tree/controller pid and pending-count snapshots before/after each install; T01-T18 and N01-N10 coverage/result matrix; counter/outcome evidence for direct selection, no replay/chaining, arbitrary order, one-shot completion, and nested direct-parent routing.
- Privacy/prohibited-path audit showing no Env/Outcome/State/callback record/fun/sensitive sentinel leaks, no generated-name/plan assertions, no sleep ordering, and no `code:load_binary/3` or code-path replacement used as release evidence.
- Exact unavailable-evidence record and closure owner for every unverified release or matrix item.

## Observable completion

All owned files implement the specified fixture, controller, topology, release harness, and scenario matrix; focused and strict available gates pass on one immutable candidate; release evidence is either real and recorded or explicitly unverified; no product/unowned change or open test defect remains; and parent review recommends merge readiness.

## Merge and correction gate

- Test changes merge only `integrate -> target` after parent review and strict verification bind to the same candidate.
- A product finding returns exact reproduction/evidence to the dispatcher. Any fix uses `task-test-3-fix-N` in coding and reaches tests only by `coding -> target -> integrate -> target`.
- No direct coding-to-integrate merge, test-side product fix, accepted assertion failure, hidden skip, stale evidence, or alternate upgrade proof is allowed.

## Parent handoff

Return `Status`, `Result`, exact base/candidate/tree, changed paths, scenario matrix, release/application/controller evidence, commands/exits/logs, unavailable items with closure, findings, and merge-readiness recommendation. Do not stage, commit, merge, update status, provision worktrees, edit excluded paths, or declare the initiative complete.
