# sf-migrate generation-2 lane manifest

- Lanes status: complete
- Plan: plan-2.md
- Concurrency: lanes
- Merge policy: merge-ready FIFO
- Merge concurrency: 1

## Control worktree

- Name: async-sf-cc
- Path: /home/slepher/project/erl_projects/async-sf-cc
- Target branch: async-sf-cc
- Target base: a980e9086a7e142c888d82f75e94d49f55334f51

## Lane coding

- Worktree name: coding
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/coding
- Branch: codex/sf-migrate-g2-coding
- Serial: true

## Lane integrate

- Worktree name: integrate
- Worktree path: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-migrate-g2-integrate
- Serial: true

## Defaults

- Default coding lane: coding
- Default integration lane: integrate
- Route: coding -> target -> integrate -> target
- Product correction route: task-test-2 finding -> task-test-2-fix-N in coding -> target -> integrate refresh/rerun -> target

## Phase task-test-2

- Kind: test
- Mode: normal
- Mode basis: canonical nested lifecycle/fixture work and the broader owner-version matrix have disjoint owned paths and review boundaries; two flat serial subtasks can focused-run independently before one immutable strict parent gate
- Execution strategy: split-def
- Worker role: def_coding_worker
- Subtasks: task-test-2.1, task-test-2.2
- Parent verification role: full_tester
- Lane: integrate
- Depends on: none
- Covers: none
- Base: a37d5335101c0d24169468f94614f3f8f7361e3d

## Subtask routing

- `task-test-2.1` starts from exact execution base `a37d5335101c0d24169468f94614f3f8f7361e3d`, owns the nested fixture and canonical flow suite, and produces a reviewed immutable checkpoint.
- `task-test-2.2` starts from that reviewed checkpoint, verifies `a37d5335101c0d24169468f94614f3f8f7361e3d` remains its target ancestor, owns only the matrix suite, and produces the cumulative reviewed checkpoint.
- Subtasks are flat contracts embedded in `task-test-2.md`; the second has a checkpoint dependency but no overlapping write ownership.
- Parent `full_tester` binds only the final reviewed cumulative checkpoint. Neither subtask may provision lanes, stage, commit, merge, or update status.

## Visibility and closure gates

- Integrate must be created or refreshed from exact execution target `a37d5335101c0d24169468f94614f3f8f7361e3d` before `task-test-2.1` begins.
- A product correction is visible to testing only after coding reaches target and integrate refreshes from target; lane-local or direct coding-to-integrate visibility is invalid.
- Test-owned changes return from integrate to target only after the strict parent gate passes on the exact cumulative checkpoint.
- Any candidate change, owned-path conflict, non-target correction, or unexplained unavailable evidence invalidates the affected checkpoint and downstream evidence.
