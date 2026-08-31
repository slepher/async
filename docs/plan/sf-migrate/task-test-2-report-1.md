# Full Test Report

- Task: `task-test-2`
- Generation: `2`
- Profile: `strict`
- Worktree: `/home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate`
- Branch: `codex/sf-migrate-g2-integrate`
- Immutable candidate: `c8fcbc2ea722193818984a27d1f9178167bc1252`
- Reviewed subtask checkpoints: `9e597478f0c7839fab494586b7148f31d72db657`, `c8fcbc2ea722193818984a27d1f9178167bc1252`
- Execution target ancestor: `c7751738ce30cba352581c6d2432038ae1edc54e`
- Changed paths: `test/async_sf_migrate_nested_fixture.erl`, `test/async_sf_migrate_nested_SUITE.erl`, `test/async_sf_migrate_nested_matrix_SUITE.erl`
- Final worktree: clean
- Environment: OTP 29 / ERTS 17.0.4, rebar3 3.25.1, Node v24.13.1, npx 11.8.0, Docker 29.6.2

## Results

| Command | Exit | Result |
|---|---:|---|
| `rebar3 compile` | 0 | passed |
| `rebar3 eunit` | 0 | 33/33 |
| `rebar3 ct` | 0 | 93/93, generation-1 and generation-2 suites included |
| `rebar3 xref` | 0 | passed |
| `rebar3 dialyzer` | 0 | passed |
| `rebar3 docker_ci` | 0 | help only; not a test pass |
| `rebar3 docker_ci run` | 1 | unavailable: `erlang:21` image absent; Docker API permission denied; no pull attempted |
| `npx openspec validate sf-migrate --strict` | 0 | valid |
| `git diff --check` | 0 | passed |

Focused evidence:

- `rebar3 ct --suite=test/async_sf_migrate_nested_SUITE.erl`: 3/3.
- `rebar3 ct --suite=test/async_sf_migrate_nested_matrix_SUITE.erl`: 7/7.
- `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl`: 13/13.
- Existing sf-do/sf-cc/linked/async_t suites passed within full CT; no skip or flaky result.

## Nested coverage

- Two independent FrameOwners (`order_flow` parent and `payment_flow` child) use literal A/B/C versions `{1,0,0}`, `{2,0,0}`, `{3,0,0}`.
- C explicitly retains A->C and B->C declarations for both owners. Public outcomes and route counters prove direct A->C, B->C, mixed order-C/payment-B rollout, and no inferred multi-hop.
- Child Outcome resumes the current order parent through its direct parent; summaries, one-shot completion, purge, unfinished callback, late reply, and duplicate-reply behavior pass.
- Matrix covers parent-converter/child-raw, parent-raw/child-converter, automatic-compatible omitted mapping, unfinished nested callback, designed failure/privacy, and sibling isolation.
- Failure Info remains bounded to `current_vsn`, `identity`, `module`, `reason`, `stored_vsn`; sensitive Env/Outcome/State/sentinel values are absent.
- Prohibited-mechanism and private-plan/bridge audits found no eager/reverse/version-graph/dependency-graph residue or private representation assertions in generation-2 tests.

## Deferred evidence

Docker/OTP 21–29 compatibility was not executed: the required `erlang:21` image was unavailable and the Docker API denied permission. This is recorded as unavailable, not passed; closure requires an authorized Docker/matrix environment and rerunning the affected full gate on an immutable candidate. Existing unnamed-action warnings are pre-existing and do not affect exits.
