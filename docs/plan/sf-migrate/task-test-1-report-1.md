# Full Test Report

- Task: `task-test-1`
- Generation: `1`
- Profile: `strict`
- Worktree: `/home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate`
- Branch: `codex/sf-migrate-g1-integrate`
- Target branch: `async-sf-cc`
- Target ancestor: `f76f9eeebe6ed05ce9ca120c251495f63be19c92`
- Immutable candidate: `701e5c240f5ed53470acf5da526be1a2c4e83ad5`
- Changed path: `test/async_sf_migrate_SUITE.erl`
- Final worktree: clean
- Environment: OTP 29 / ERTS 17.0.4, rebar3 3.25.1, Node v24.13.1, npm/npx 11.8.0, Docker 29.6.2, act 0.2.88

## Results

| Command | Exit | Result |
|---|---:|---|
| `rebar3 ct --suite async_sf_migrate_SUITE` | 0 | 13/13 |
| `rebar3 eunit --suite async_sf_transform_tests` | 0 | 21/21 |
| `rebar3 ct --suite async_sf_SUITE` | 0 | 5/5 |
| `rebar3 ct --suite async_t_SUITE` | 0 | 28/28 |
| `rebar3 ct --suite async_histories_SUITE` | 0 | 4/4 |
| `rebar3 ct --suite async_sf_cc_SUITE` | 0 | 9/9 |
| `rebar3 ct --suite async_sf_cc_macro_SUITE` | 0 | 4/4 |
| `rebar3 ct --suite async_sf_cc_linked_SUITE` | 0 | 4/4 |
| `rebar3 ct --suite async_sf_do_linked_SUITE` | 0 | 4/4 |
| `rebar3 compile` | 0 | passed |
| `rebar3 eunit` | 0 | 33/33 |
| `rebar3 ct` | 0 | 83/83, migration suite included |
| `rebar3 xref` | 0 | passed |
| `rebar3 dialyzer` | 0 | passed |
| `rebar3 docker_ci` | 0 | help only; no Docker test run |
| `npx openspec validate sf-migrate --strict` | 0 | valid |
| `git diff --check` | 0 | clean |

## Audits and mapping

- Prohibited-symbol audit found no sf-migrate eager scan, release-handler hook, version registry, dependency graph, or reverse migration mechanism.
- Failure/privacy cases passed with exactly `current_vsn`, `identity`, `module`, `reason`, and `stored_vsn`; sensitive Env/Outcome/State/sentinel values were absent.
- Compiler/declaration and exact routing: `declaration_compiler_contract`, `compiler_extended_matrix`, `compiler_negative_matrix`.
- Raw bridge, purge, lifecycle, and direct-parent continuity: `raw_bridge_and_purge`, `multi_owner_linked_migration`.
- Fallback, converter, designed failure, and privacy: `fallback_and_failure_privacy`, `converter_and_precedence`, `converter_env_and_precedence`, `converter_observable_matrix`, `converter_incompatible_shapes`, `converter_failure_privacy`.
- Downgrade and no reverse/appdown behavior: `downgrade_and_existing_boundary`, `downgrade_created_in_b`.
- Existing sf-do/sf-cc and linked regression behavior: full Common Test plus the focused suites above.

## Deferred evidence

`rebar3 docker_ci` only printed available tasks; the Docker/OTP compatibility matrix was not executed. Existing warnings were two unnamed non-final `sf_do` warnings from `test/sf_do_fixture.erl`. No non-zero, skip, flaky, or product failure was observed.
