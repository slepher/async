# task-test-3 review 4

## Status

审查完成。review-3 的 F1/F2/F3 已由候选闭合，但严格验证仍有一个 test-owned blocking finding。

## Task

- Task: `task-test-3`
- Kind: `task-test`
- Owned paths: `test/app_fixtures/**`、`test/test_async_controller.erl`、`test/async_sf_migrate_application_SUITE.erl`
- Review artifact: `docs/plan/sf-migrate/task-test-3-review-4.md`

## Reviewed commit

- Candidate: `92f0b397d1c2929234a962c6a5139fc9b872eae5`
- Tree: `c56c5ab37984a73694f230ed0e2ad5f89a2f6d7c`
- Branch/HEAD: `codex/sf-migrate-g3-integrate` / `92f0b397d1c2929234a962c6a5139fc9b872eae5`
- Contract base: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- Implementation range: `2e06985948c185ee60c0f2afd7d4f03fa031d17b..92f0b397d1c2929234a962c6a5139fc9b872eae5`
- Candidate was clean before this artifact was written. The implementation range changes only the assigned owned paths; the preceding `2e069859...` commit owns the dispatcher plan/contract files.

## Evidence

### review-3 closure

- **F1 closed — N06/N07 deterministic direct-parent checkpoint.** `order_flow` records the generated direct `ParentRef` at nested start, calls `async_m:run_info/3` for the exact `{ParentRef, Outcome}` message, suppresses `unhandled`, removes the checkpoint with `maps:take/2`, and only then emits public `{parent_consumed, Tag}`. N06 and N07 wait for that event before asserting the intermediate exact maps. The assertions prove child completion, direct-parent consumption, outer incompletion, one remaining controller MRef, and exact route/resume/converter ownership before the outer reply.
- **F2 closed — N08 A/B/C exact raw counters.** N08 creates nested A, B, and C frames, replies in C/B/A order, checks each progress boundary, and ends with exact full maps: order request/action `3`, A->C route `2`, B->C route `2`, resume `6`; payment request/action `3`, A->C route `1`, B->C route `1`, resume `3`. Exact map equality also proves no converter, A->B, replay, or other historical counter ran.
- **F3 closed — T14/N10 exact controller and state maps.** T14 proves the first reply completed once, duplicate reply returned `{error, already_replied}`, counters stayed exact, and controller counts were exactly `#{pending => 0, replied => 1}`. N10 proves after the early parent reply the exact intermediate order/payment/controller maps, after the child direct-parent consumption the exact final maps, and no map change or second child/outer completion after the duplicate reply.

### Application/release and protocol evidence

- OTP `29`, ERTS `17.0.4`, rebar `3.25.1`.
- Focused command `rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`: exit `0`, `6/6` CT cases passed, `0` skipped. Log: `_build/test/logs/ct_run.nonode@nohost.2026-08-31_20.39.18/lib.async.async_sf_migrate_application_SUITE.logs/run.2026-08-31_20.39.18/suite.log`.
- The focused log records successful `systools:make_script`, `systools:make_relup`, and `systools:make_tar` generation plus 35 successful calls each to `release_handler:unpack_release`, `check_install_release`, and `install_release`. It includes A->B->C and fresh A->C transitions, `which_releases` checks, stable application/tree/controller identities, exact pending identities, and release artifact paths/hashes. No `code:load_binary/3`, release code-path replacement, or sleep-based ordering exists.
- `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl,test/async_sf_migrate_nested_SUITE.erl`: exit `0`, `16/16` passed.
- `rebar3 ct`: exit `0`, `99/99` passed. This gives a second passing execution of the application/release suite on the same candidate.
- `rebar3 xref`: exit `0`.
- `rebar3 dialyzer`: exit `0`.
- `git diff --check`: exit `0`.
- Static privacy/protocol audit found only public request, completion, counter, version, pid/tree, pending identity, and bounded error observations. Unknown and duplicate MRefs remain distinct; replied identities are retained without exposing callback records, Env, generated bridge names, plans, or runtime-private state. N08's sentinel is absent from public outcomes. No source/test outside owned paths is changed by the implementation range.

## Findings

### F4 — blocking — static fixture source discovery breaks the required EUnit gate

- **Immutable evidence:** on candidate `92f0b397...`, `rebar3 eunit` exits `1` after reporting `Module 'order_flow' not found in project`, `payment_flow`, `sf_fixture_app`, and `sf_fixture_sup`, repeated for all A/B/C fixture directories (12 missing-module reports).
- **Contract basis:** `task-test-3.md` lists `rebar3 eunit` under blocking self-validation and states that any available-command failure blocks merge readiness. This is an available runner failure, not unavailable external evidence.
- **Cause:** the new static fixture files are ordinary `test/app_fixtures/{a,b,c}/*.erl` sources, so EUnit discovers their duplicate module names, while the suite only compiles them dynamically into CT private directories. They therefore cannot be loaded as project EUnit modules.
- **Correction owner and allowed paths:** `task-test-3`, limited to `test/app_fixtures/**` and `test/async_sf_migrate_application_SUITE.erl`.
- **Bounded correction:** keep the exact A/B/C application names, module names, version metadata, appup/relup behavior, controller separation, and all F1/F2/F3 assertions, but store the dynamically compiled fixture module sources in a form EUnit does not auto-discover (for example, owned non-`.erl` templates copied to CT private `.erl` files before `compile:file/2`). Do not modify read-only `rebar.config*` or weaken EUnit selection.
- **Focused validation and observable closure:** `rebar3 eunit` exits `0` with no missing fixture modules; then rerun the focused application suite, adjacent migration suites, full `rebar3 ct`, xref, dialyzer, and `git diff --check` on the new immutable candidate.

## Verdict

`changes_required`

F1/F2/F3 are closed and the real OTP release/application evidence passes, but F4 violates an explicit blocking gate, so this candidate is not merge-ready.

## Changes

Only this review artifact was added. No product source, test source, `test.md`, workflow status, index, or commit was modified.

## Caveats

- The optional OTP 21–29 Docker matrix was not run. That absence does not cause this verdict; real OTP 29 release evidence is available and passed.
- `test.md` is absent from this worktree. The static review used the accepted `task-test-3.md`, `context-3.md`, `plan-3.md`, visible T01–T18/N01–N10 mapping, and executable public observations; no decisive semantic question remained.
