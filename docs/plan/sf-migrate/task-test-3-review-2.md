# task-test-3 Parent Semantic Review 2

Status:
review_complete

Task:
`task-test-3`（`task-test` 第二轮 parent semantic review）

Reviewed commit:

- Candidate: `add2e9f914103b1bf3fcb3600f492aa09fd5b356`
- Tree: `4a157ff42d46bcf56e7702d64981943ba209197b`
- Branch: `codex/sf-migrate-g3-integrate`
- Worktree: `/home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate`
- Correction base / candidate parent: `8057871c049879fa53f89e73149f7cf4f55130ae`
- `HEAD`、branch ref 与 candidate 均精确解析为 `add2e9f914103b1bf3fcb3600f492aa09fd5b356`；candidate 的唯一 parent 是 `8057871c049879fa53f89e73149f7cf4f55130ae`。审查前后 candidate worktree 均 clean。

Evidence:

- 相对 `8057871` 的 correction scope 仅包含 7 个 owned test files：B/C 的 `order_flow.erl`、B/C 的 `payment_flow.erl`、共享 `order_flow.hrl`、共享 `payment_flow.hrl` 与 `test/async_sf_migrate_application_SUITE.erl`；无产品、配置、既有测试或文档进入 candidate。`git diff --check 8057871..add2e9f` exit `0`。
- N02 的事件已改为先在 A 回复 payment 并观察 A child completion，再安装 B、保持 order MRef，最后在 B 回复 order；最终 order/payment completion、callback pending 与 controller replied 数量均精确闭合为一次/零 pending。
- T06/T07 现以完整 counter map 等值证明 request/action/resume 与 A→C route 各为 `1`，并明确证明 A→B route 与 converter key 不存在。T11-T13、T17-T18 现以完整 counter map、逐 tag completion、controller pending/replied 等值证明 A/B/C stored owner 的 route 与 raw-current frame 数量。
- `install_release/2` 已在 `unpack_release` 前取得真实 `Before = observe(N)`，在 `install_release` 与 `which_releases` 后取得真实 `After = observe(N)`；snapshot 保存五个 tree pid、controller pid/count/pending，以及每个 MRef 的精确 `{pending,{Pid,MRef}}`，并在安装后逐项比较。日志中的 A→B 示例保留同一 order worker `<34712.101.0>`、controller `<34712.110.0>` 与 MRef identity。
- N08 已增加两个可区分的 parent/child pair、逐步 pending/completion 断言和唯一 `{privacy_sentinel,Ref}`；通过的 reply 顺序证明 PB 只能完成 B sibling、PA 只能完成 A sibling，且 B child/order outcome 均不含 sentinel。其版本混合仍见 F2。
- focused command：`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`，exit `0`；`6 ok, 0 failed, 0 user_skipped, 0 auto_skipped`。日志：`_build/test/logs/ct_run.nonode@nohost.2026-08-31_19.52.55/lib.async.async_sf_migrate_application_SUITE.logs/run.2026-08-31_19.52.55/suite.log`。
- 同次日志记录 OTP `29`、ERTS `17.0.4`、rebar3 `3.25.1`；A/B/C 的 `systools:make_script/2`、B/C 的 `systools:make_relup/4`、A/B/C 的 `systools:make_tar/2` 均记录路径和成功返回。每个 release node 均记录 exec、args、env、`peer:start_link` 返回和初始 public status。
- 同次 application/nested case 日志记录每次 `release_handler:unpack_release/1`、`check_install_release/1`、`install_release/1`、`which_releases/0` 及 before/after snapshot。真实 direct A→C 的 check/install 返回 `{ok,"1.0.0",[]}`，A→B 与 B→C 分别返回前一 release version。
- 同次 artifact 日志记录 `.rel`、`.script`、`.boot`、`relup`、tar 的绝对路径、存在性与 SHA-256。B `relup` 为 `FFD21FD49AB30F3C800DD0D9A9C65BDA10FF5F5EA4F082CF368BF6C656455503`，C `relup` 为 `BBBB8F5E4E8E7598AB26D9261B48BD6A183B7B58E79B71E4219EF59C4831FDB3`；A 是基线 release，因此其 `relup` 明确记录为不存在，B/C `relup` 均被断言存在。
- correction scope 中未发现 sleep、`code:load_binary/3`、`code:set_path/1`、`code:del_path/1` 或 `code:replace_path/2`。唯一 `code:add_patha/1` 仍只用于独立 node 启动前放入单一 fixture ebin，不参与 release upgrade 或 upgrade proof。

Findings:

## F1 — N06/N07 没有确定性证明 child 已先进入 direct parent（blocking）

固定证据：

- `add2e9f:test/async_sf_migrate_application_SUITE.erl:266-280` 在 `complete_payment/2` 返回后立即释放 order MRef，没有 parent-consumed checkpoint。
- `add2e9f:test/app_fixtures/payment_flow.hrl:57-61` 先向 suite observer 发送 `{payment_complete,...}`，之后才向 `ParentPid` 发送 `{ParentRef,Outcome}`。因此 suite 收到 child completion 时，parent_ref 甚至不保证已经发送；随后 controller 与 payment worker 从不同 sender 向 order worker 发消息，二者到达顺序没有保证。
- N06/N07 的最终 route/resume/converter 总数在两种到达顺序下都相同，故当前绿色结果不能区分“child first”与“parent MRef first”。

合同依据：

`test.md` N06 要求 child raw outcome 先到 parent、再由当前 parent converter 处理；N07 要求 child converter 结果作为 parent current step 输入。场景标签必须由可观察事件顺序证明，不能依赖调度偶然性。

修正 owner：

`task-test-3` test correction owner；当前没有产品/runtime finding。

Allowed paths:

首选 `test/async_sf_migrate_application_SUITE.erl`；只有为提供确定性的 public parent-consumed checkpoint 所必需时，允许最小修改 `test/app_fixtures/order_flow.hrl`、`test/app_fixtures/payment_flow.hrl` 及对应 B/C wrapper，不得修改产品。

Preserved invariants:

保持 direct `parent_ref`、真实 A→B/A→C release、外部 controller、精确 MRef、公开黑盒观察、无 sleep、无私有 callback/plan/bridge 读取、无 code-path upgrade substitute。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

N06/N07 在释放 parent MRef 前，通过确定性的 public checkpoint 证明 child outcome 已被对应 order callback 消费、非目标 sibling 未变化、outer completion 仍为零，并对该中间点的 route/resume/converter 数量做精确等值；随后释放 parent MRef并证明最终 counters、controller、callback 与 completion 各只闭合一次。

## F2 — N08 仍未制造 A/B/C mixed pending ownership（blocking）

固定证据：

- `add2e9f:test/async_sf_migrate_application_SUITE.erl:289-297` 在 A current 时连续创建两个 nested pair，然后直接 A→C；OA/OB/PA/PB 的 stored owner 全是 A。
- 当前场景已证明两个 sibling 和 sentinel privacy，但没有 B-owned 或 C-owned pending frame，也没有 A/B/C frame 的逐 owner route/raw counter closure。

合同依据：

`test.md` N08 明确要求 order A→B→C、payment A/B/C mixed pending、乱序回复与 sibling isolation；accepted contract 还要求每个 frame 使用自身 stored version。

修正 owner：

`task-test-3` test correction owner。

Allowed paths:

仅 `test/async_sf_migrate_application_SUITE.erl`。

Preserved invariants:

保留现有两个以上可区分 sibling、唯一 privacy sentinel、direct parent routing、逐步 pending/completion 观察、真实 release 安装和 one-shot closure。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

N08 在 A、B、C current 阶段创建可区分的 pending parent/child frame，C current 后按非创建顺序回复；每一步证明仅目标 sibling 改变，A/B frame 分别命中精确 source route、C frame 命中 raw current body，request/resume/completion/controller counters 全部精确，且所有公开 outcome 不含 sentinel。

## F3 — T14/T16 与 N10 仍缺少完整 exact-counter closure（blocking）

固定证据：

- `add2e9f:test/async_sf_migrate_application_SUITE.erl:200-215` 的 T14/T16 仍调用 `assert_done/4`；该 helper 只要求 route key 存在、completion count 为 `1` 和 controller pending 为 `0`，不比较完整 request/action/resume/route/converter map，也不比较 controller replied exact count。
- `add2e9f:test/async_sf_migrate_application_SUITE.erl:317-327` 的 N10 以 duplicate controller rejection、mailbox no-completion 与 `assert_nested_done/3` 收尾；它证明 order/payment completion 各为一次，却没有证明 parent route/resume 没因 late/duplicate child 多执行。
- T15 的完整 public snapshot before/after 等值已足以证明 unknown reply 不改变状态；T11-T13、T17-T18 的完整 map 等值已关闭，不属于本 finding。

合同依据：

`test.md` T11-T18 要求每帧 request、stored-owner route、current body/converter、completion 和 controller exact-once；N10 还明确要求无重复 parent continuation 或 outer completion。

修正 owner：

`task-test-3` test correction owner。

Allowed paths:

仅 `test/async_sf_migrate_application_SUITE.erl`。

Preserved invariants:

保持现有 T14 duplicate/unknown 区分、T16 install continuity、N10 parent-first/child-late 时序、公开 counters 与严格断言；不得通过放宽预期或只检查 key presence 闭合。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

T14、T16 对完整 counter map、completion count、controller pending/replied 做精确等值；N10 在 parent-first 中间点与 child/duplicate 后分别比较 order/payment route、resume、converter、request、callback、completion 和 controller exact counts，证明 duplicate child 没有第二次 payment body、parent continuation 或 outer completion。

## F4 — install snapshot 的 `application` 字段仍不是 application pid（blocking）

固定证据：

- `add2e9f:test/async_sf_migrate_application_SUITE.erl:371-389` 将 `application` 设为 `application_identity(Apps)`。
- `add2e9f:test/async_sf_migrate_application_SUITE.erl:587-589` 的 `application_identity/1` 固定返回 atom `sf_fixture`；`assert_release/5` 因而只比较 `sf_fixture =:= sf_fixture`。
- 同次 focused 日志的 before/after snapshot 明确显示 `application => sf_fixture`，不是 pid。五个 supervisor/worker pid 与 controller pid 的连续性已经真实证明，但上一轮 F3 要求的 application pid 连续性仍未闭合。

合同依据：

`test.md` 启动与升级 checkpoint、accepted task contract 及上一轮 F3 observable closure 要求安装前后保存并比较 application pid、完整 tree pid、controller pid/state 和精确 `{Pid,MRef}`。

修正 owner：

`task-test-3` test correction owner。

Allowed paths:

仅 `test/async_sf_migrate_application_SUITE.erl`，使用当前 OTP/fixture 已有的 bounded public observation；不得增加私有 release-state reader。

Preserved invariants:

保持当前真实 before/after 取样位置、五个 tree pid、controller/count/pending/identity 精确比较、application availability/version 查询和真实 `release_handler` 流程。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

每次 install 前后 snapshot 的 `application` 字段为真实 pid，并与五个 tree pid、controller pid 及每个 `{Pid,MRef}` 一起精确相等；日志可直接恢复这些 before/after 值，同时继续证明 application version 已切换且 tree 可查询。

Verdict:
`changes_required`

Changes:

仅新增本 review artifact：`docs/plan/sf-migrate/task-test-3-review-2.md`。未修改 candidate 的测试/产品文件，未 stage、commit、merge 或更新 workflow status。

Caveats:

- focused suite 的 `6/6` 绿色证明当前实现和断言可运行，但不关闭 F1-F4 的语义覆盖缺口。
- 本轮未运行 `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl,test/async_sf_migrate_nested_SUITE.erl`、完整 `rebar3 ct`、EUnit、xref、dialyzer 或可选 OTP matrix；已有 blocking semantic findings，修正后的新 immutable candidate 仍须绑定合同要求的全部可用 gate。
- focused 运行只在 ignored `_build/test` 下生成日志和 release artifacts；candidate source tree 在运行后保持 clean。
