# Test Task Contract

- Task: task-test-1
- Bundle generation: 1
- Phase artifact: task-test-1.md
- Plan: plan-1.md
- Lane manifest: lanes-1.md
- Context: none
- Evidence target: task-coding-3 accepted merge on target branch async-sf-cc containing task-coding-1 and task-coding-2
- Task kind: test
- Mode: normal
- Mode basis: 一个边界明确的 public black-box test-asset subtask 可独立写入并 focused-run；之后保持不变的完整 repository/OTP 验证在其不可变 checkpoint 上只读执行
- Execution strategy: split-def
- Worker role: def_coding_worker
- Parent verification role: full_tester
- Test profile: strict
- Lane: integrate
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-migrate-g1-integrate
- Base: task-coding-3 accepted merge on target branch async-sf-cc
- Depends on: none
- Covers: task-coding-1, task-coding-2, task-coding-3

## Bounded goal

由 fresh `def_coding_worker` 针对一个 target-visible candidate 独立增加并 focused-run document-driven public black-box tests。不可变 checkpoint 及 test review 通过后，parent `task-test-1` 由 `full_tester` 在该 checkpoint 上只读验证，证明全部 OpenSpec sf-migrate scenarios，并保持已实现的 sf-do/sf-cc boundary。测试可通过 public/compiler/runtime seams 观察行为，但不得修改 production code，也不得把 private opaque plan/bridge shapes 编码为 acceptance。

## Coverage objectives

1. Compiler/declaration: explicit owner Vsn required; exact literal term/no coercion; clauses/case for multiple Vsns; auto-export; placement/syntax; complete named historical input; partial/unnamed rejection; duplicate source; name-only mapping; missing Block/Step; collision; bounded diagnostics without AST/payload.
2. Raw bridge: Block/Step rename, compatible Outcome/capture subset, converter-required diagnostics, purged old code, no old body execution, no request replay, no callback/frame/store mutation, and direct parent continuity.
3. Explicit/fallback: mapped explicit bridge before same-identity automatic behavior; missing router, `unhandled`, and unmapped unchanged Step fall back; matched invalid bridge never falls back; unknown/incompatible source fails structurally.
4. Converter: Outcome-only, Env-only, combined, capture rename/deletion, same-name semantic value conversion, extra Env, converter rejection, invalid return, incompatible Outcome/Env, and `error`/`exit`/`throw` preserving existing user-code exception behavior.
5. Failure/privacy: exact outer shape, bounded required Info, direct `parent_ref`, current node only, failure handler behavior, no Env/Outcome/State/callback/fun/sensitive sentinel leakage, and controlled business Reason behavior consistent with README.
6. Downgrade: A callback survives A->B->A; B-created compatible frame resumes in A with extra Env ignored and original Outcome; missing A capture/Step fails only on reply; no reverse converter/target candidate/appdown scan/fail/blocking.
7. Integration/regression: linked nodes with different owners/Vsns resolve independently; stable/ordinary mixed callbacks, timers/monitors/request lifecycle, partial-release window, applications without sf-migrate, unnamed same-version resume, current manifest/store/record shape, and all existing suites remain unchanged.

## Existing coverage to reuse

- Existing `async_sf_SUITE`, `async_sf_do_linked_SUITE`, `async_sf_cc_SUITE`, and `async_sf_cc_linked_SUITE` remain regression authority for current identity/capture, direct-parent linkage, one-shot node consumption, cleanup, ordinary/stable separation, and callback summary privacy.
- Do not duplicate those assertions as sf-migrate tests unless a real cross-version bridge changes the observable. New coverage must exercise historical declarations, exact Vsn routing, purge, conversion, or downgrade.

## Owned paths

- `test/async_sf_migrate_SUITE.erl` (new primary suite)
- `test/async_sf_migrate_fixture.erl` (new shared fixture only if the suite cannot keep dynamic version source readable)
- `test/async_sf_transform_tests.erl` (only for public `format_error/1` cases not cleanly covered by compiler fixture diagnostics)

All `src/`, `include/`, README, OpenSpec, existing unrelated test suites/fixtures, and workflow paths are read-only. Prefer dynamic compilation/loading of exact A/B FrameOwner versions inside the primary suite over a family of speculative fixture modules. A production defect is a finding, not authority to fix it here.

## Test design constraints

- Assert public compiler diagnostics, callback Outcomes, side effects/counters, State before/after summaries, and exact code load/purge behavior; do not assert opaque plan term shape or generated private bridge atom.
- Use sensitive sentinel terms and execution counters to prove no payload leak, old qualifier replay, request resend, reverse converter, or callback mutation.
- Freeze and report the exact target/integrate candidate before running. A product fix or candidate change invalidates prior results.
- Keep expected incompatibility separate from converter/router user exceptions; tests must observe native exception class where the existing harness exposes it.
- Do not weaken existing tests, mark new product failures expected, or modify production/docs to make a test pass.

## Subtask task-test-1.1

- Worker role: fresh `def_coding_worker`
- Session: fresh，且未实现任何被覆盖的 coding task
- Lane: integrate
- Depends on sibling subtasks: none
- Visibility dependency: parent `Covers` gate; `task-coding-1`, `task-coding-2`, and `task-coding-3` must be accepted on target and visible in integrate before dispatch
- Covers coding work: `task-coding-1`, `task-coding-2`, `task-coding-3`
- Normative scope: 本 parent 的 `Coverage objectives`、`Existing coverage to reuse`、`Owned paths` 和 `Test design constraints`
- Observable objective: 只增加本 parent 定义的 public black-box sf-migrate acceptance assets，并 focused-run 每个新增或修改的 test。
- Owned paths: `test/async_sf_migrate_SUITE.erl`, `test/async_sf_migrate_fixture.erl`, and, only when public `format_error/1` coverage cannot cleanly live in compiler fixtures, `test/async_sf_transform_tests.erl`
- Product authority: none；product failure 必须以 exact evidence 返回，subtask 不得修复。

### Required work

1. Record covered target commit and run existing focused baselines before adding fixtures.
2. Build the compiler/declaration/raw bridge matrix, including exact A/B source, old-code purge, and negative diagnostics.
3. Add converter/precedence/designed-failure/exception/privacy cases with invocation and sensitive-payload sentinels.
4. Add A/B/A and B->A compatible/incompatible/no-side-effect cases using the actual loaded manifest and original reply Outcome.
5. Add multi-owner linked, ordinary/stable mixed, partial-window, and non-user regression cases only where existing suites do not already prove them.
6. 在 unchanged candidate 上运行 focused self-run，只修正 test-owned scenario、fixture 或 harness failure。
7. 为 checkpoint/review 返回 requirement/scenario-to-case matrix、exact commands/exits、changed-path evidence，以及所有 product-owned 或 unavailable result。

### Focused self-run

- `rebar3 eunit --module=async_sf_transform_tests`
- `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl`
- `rebar3 ct --suite=test/async_sf_SUITE.erl,test/async_sf_do_linked_SUITE.erl,test/async_sf_cc_SUITE.erl,test/async_sf_cc_linked_SUITE.erl`

每个新增或修改的 test 都必须在该 self-run 中执行。返回每条 command、exit 和 result；不得声称已完成 parent/full verification。

### Checkpoint, review, and parent gate

- focused self-run 通过后，由 dispatcher 创建不可变 integrate-lane checkpoint commit；worker 不 stage/commit。
- lightweight test-subtask review 在该 checkpoint 上检查 frozen public objective、nonduplicated observable coverage、failure ownership、assertions 和 focused-run evidence；不执行 implementation-simplicity review。
- 只有 checkpoint review 通过，`task-test-1.1` 才完成；checkpoint 本身不代表 parent acceptance 或 merge readiness。
- 此后 parent `task-test-1` 才能在该 exact checkpoint 上 dispatch `full_tester` 执行下述完整只读验证。任何 candidate change 都使 parent run 失效，并须重新通过同一 checkpoint/review gate。

### Done when

- frozen coverage objectives 所需的 owned test assets 完整，每个 changed test 通过 focused self-run，owned paths 外无改动，且 immutable checkpoint review 通过。
- Product findings 和 unavailable environment evidence 均被精确返回；subtask 不得隐藏、接受或修复它们。

### Stop conditions

- parent visibility dependency 未满足、candidate identity 改变、存在 owned-path conflict，或完成工作需要修改任何 production、OpenSpec、workflow、configuration 或 unrelated test。
- focused case 暴露 product defect、privacy leak、callback mutation/replay、swallowed exception，或其他无法完全在 owned public test asset 内修正的 failure。

## Parent full_tester verification

`full_tester` 只能从 reviewed immutable `task-test-1.1` checkpoint 启动。它验证 worktree identity 和 HEAD，不修改 source/test/workflow/configuration，并在该唯一 candidate 上执行完整 parent commands。

- Task ID: task-test-1
- Generation: 1
- Source branch: async-sf-cc
- Test branch: codex/sf-migrate-g1-integrate
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Target commit: dispatcher 在 dispatch 时绑定 reviewed immutable `task-test-1.1` checkpoint 的 exact commit
- Test profile: strict
- Timeout: 60 minutes per command

## Blocking self-validation

- Commands: `rebar3 compile`; `rebar3 eunit`; `rebar3 ct`; `rebar3 xref`; `rebar3 dialyzer`; `rebar3 docker_ci`; `npx openspec validate sf-migrate --strict`; `git diff --check`; bounded prohibited-symbol and failure-payload privacy audits

只有该只读 run 返回足以满足现有 merge/final acceptance gates 的 exact command、exit、environment、output/artifact 和 unavailable-evidence result，parent gate 才通过。`full_tester` 只报告 failure，不修改 tests 或 product code。

## Merge and correction gate

- Test changes become merge-ready only when focused and full commands pass on the same immutable candidate with no unexplained warning, skip, flaky result, or product finding.
- On a product finding, stop and return exact reproduction/evidence. The dispatcher creates `task-test-1-fix-N` in coding from the observed target; correction flows coding -> target, integrate refreshes from target, then affected and full tests rerun. No direct coding-to-integrate merge.
- Environment-unavailable OTP/OpenSpec tooling is reported as deferred evidence with a named owner and cannot be silently counted as passing or completion.

## Required evidence

- Exact target base, integrate candidate/tree, changed test paths, and command lines/exits/environment.
- Every sf-migrate requirement/scenario mapped to a concrete case or explicit existing regression case.
- Code load/purge proof; converter/old-body/request invocation counts; callback/frame summary before/after; direct-parent/failure-handler observations; sensitive payload no-leak assertion.
- A/B/A and B-created-frame down traces showing no appdown mutation or reverse call.
- Prohibited mechanism search and retained-match explanations; full-suite/OTP/OpenSpec reports and any unavailable evidence owner/closure.

## Observable completion

- All normative scenarios pass on one immutable integrated candidate, with real exact-version/purge/converter/downgrade behavior rather than private-plan assertions.
- Existing sf-do/sf-cc/ordinary behavior, record/store/current manifest shape, direct-parent lifecycle, and privacy regressions all pass unchanged.
- No product finding, unowned edit, accepted failure, hidden skip, eager/reverse/release residue, or open deferred evidence remains.

## Stop conditions

- Any covered coding task is absent from target/integrate or candidate identity changes during testing.
- A fixture requires production/docs/OpenSpec changes or a private opaque representation must be standardized to write the test.
- A product defect, sensitive leak, callback mutation/replay, swallowed exception, non-zero full gate, or unexplained baseline failure appears.
- Required environment/tooling is unavailable and no authorized closure owner exists.

## Handoff

`task-test-1.1` 返回 `Status`、exact subtask、immutable candidate、test diff、focused self-run、coverage matrix、findings 和 checkpoint-review evidence。parent 的 `full_tester` 返回绑定 reviewed checkpoint 的只读 complete-validation evidence。两者都不得修复 product code、stage/commit/merge、编辑 docs/specs/workflow/status，或宣布 initiative complete。
