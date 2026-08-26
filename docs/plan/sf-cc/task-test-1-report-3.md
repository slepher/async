# Test Task Report

Status: complete

Task: task-test-1

Candidate: `aab6356893524e0cb36293837a4c995d6a33769e` (tree `adad11209e224d298779ba53d1d9e187d5806a82`)

## Routing

- Task: task-test-1.md
- Report: task-test-1-report-3.md
- Scope: parent-complete
- Checkpoint: aab6356893524e0cb36293837a4c995d6a33769e
- Target: aab6356893524e0cb36293837a4c995d6a33769e
- Command: rebar3 ct --suite async_sf_cc_SUITE; rebar3 ct --suite async_sf_cc_macro_SUITE; rebar3 ct --suite async_sf_SUITE; rebar3 ct --suite async_t_SUITE; rebar3 compile; rebar3 eunit; rebar3 ct; rebar3 xref
- Result: passed

## Result

- R15 已关闭：`old_three_tuple_unavailable` 保留精确 `{legacy_outcome, purged_reply, PurgedClean}` assertion 并通过；public `async_m:handle_info/3` 与 runtime 均保留 tagged result。
- Focused suites 全部通过；full CT 64/64 passed；无 product-owned、test-owned 或 unknown 新 failure。
- owned test changes 精确限定为 `test/async_sf_SUITE.erl`、`test/async_t_SUITE.erl`、`test/async_sf_cc_SUITE.erl`、`test/async_sf_cc_macro_SUITE.erl`；本轮未修改文件、未 stage/commit。

## Validation

| Command | Exit | Result |
|---|---:|---|
| `rebar3 ct --suite async_sf_cc_SUITE` | 0 | 10/10 passed；R15 exact tag passed |
| `rebar3 ct --suite async_sf_cc_macro_SUITE` | 0 | 3/3 passed |
| `rebar3 ct --suite async_sf_SUITE` | 0 | 10/10 passed |
| `rebar3 ct --suite async_t_SUITE` | 0 | 25/25 passed |
| `rebar3 compile` | 0 | passed |
| `rebar3 eunit` | 0 | 12/12 passed |
| `rebar3 ct` | 0 | 64/64 passed |
| `rebar3 xref` | 0 | passed |
| `git diff --check` | 0 | clean |

EUnit/full CT still emit two existing unnamed `sf_do` non-migratable warnings; no unexplained warning or regression was observed in the available matrix. Common Test report: `_build/test/logs/index.html`.

## Coverage

- R1–R19 now have direct observable coverage in the new runtime/macro suites plus retained ordinary/frame suites: runner modes/context; CcRef adoption/store/scope; macro gate; cleanup-before-invoke/exactly-once; message/reply/timeout/DOWN; structural and CcRef-only terminals; real old tuple resumable/unavailable outcomes; matcher privacy/atomicity; active-fail dedupe; literal Vsn.
- The nine original deferred failures remain closed in their owned fixtures and expectations.

## Unavailable or not-passed checks

- `rebar3 dialyzer`: prior exit 1 with 140 product/dependency warnings; not rerun in this correction and not claimed passed.
- `npx openspec validate sf-cc --strict`: unavailable because no OpenSpec executable is resolvable; not claimed passed.
- OTP 21–29 compatibility matrix / `rebar3 docker_ci run`: unavailable because required Docker images (including `erlang:21`) are absent; worker is not authorized to pull/install them.

## Closure

All available focused/full checks and all owned coverage pass, including the R15 public tag closure. Environment-limited/non-passing Dialyzer, OpenSpec, and OTP checks remain explicit open evidence and are not represented as passing task-test validation.
