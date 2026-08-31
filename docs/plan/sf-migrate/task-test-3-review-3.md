# task-test-3 Parent Semantic Review 3

Status:
review_complete

Task:
`task-test-3`（`task-test` 第三轮 parent semantic review）

Reviewed commit:

- Candidate: `671225b3697cccc8268b1f14a77e704a465ce5ce`
- Tree: `f9a2e236f973730871c233ef4793016a63d6fbd3`
- Branch: `codex/sf-migrate-g3-integrate`
- Worktree: `/home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate`
- Previous candidate / candidate parent: `add2e9f914103b1bf3fcb3600f492aa09fd5b356`
- Accepted base: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- `HEAD` 与 branch ref 均精确解析为 candidate；candidate 的唯一 parent 是上一候选，accepted base 是 candidate 祖先。审查前、focused run 后的 candidate worktree 均 clean。

Evidence:

- 上一候选到 candidate 的 owned-only delta 仅修改 `test/async_sf_migrate_application_SUITE.erl`，共 `42 insertions, 22 deletions`；没有产品、配置、既有测试、fixture 或文档进入 correction commit。`git diff --check add2e9f914103b1bf3fcb3600f492aa09fd5b356..671225b3697cccc8268b1f14a77e704a465ce5ce` exit `0`。
- Candidate 仍以可见的一一对应 matrix 顺序执行 T01-T18 与 N01-N10。T06/T07 的完整 order counter map 精确为一次 request/action/resume、一次 A→C route，且 A→B route 与 converter key 均不存在；T11-T13、T17-T18 的完整 map、逐 tag completion 与 controller closure 继续保留。T16 本轮已增加完整 counter map及 controller `#{pending => 0, replied => 1}` 等值。
- Review-2 F4 已关闭：`671225b:test/async_sf_migrate_application_SUITE.erl:392-412` 读取并断言真实 application master pid；`assert_release/5` 将它与五个 tree pid、controller pid/count/pending 及每个精确 `{Pid,MRef}` 一同做 before/after 等值。focused 日志中的 N02 A→B snapshot 保持 application `<34768.97.0>`、tree `<34768.99.0>` 至 `<34768.103.0>`、controller `<34768.110.0>` 以及 `{<34768.101.0>,MRef}` 不变，application marker/version 从 A/`1.0.0` 切换到 B/`2.0.0`。
- `rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`：exit `0`；`6 ok, 0 failed, 0 user_skipped, 0 auto_skipped`。日志根：`_build/test/logs/ct_run.nonode@nohost.2026-08-31_20.10.26/lib.async.async_sf_migrate_application_SUITE.logs/run.2026-08-31_20.10.27/`；`suite.log` 明确记录 `6 ok, 0 failed of 6 test cases`。
- 同次日志记录 OTP `29`、ERTS `17.0.4`、rebar3 `3.25.1`；A/B/C 的 `systools:make_script/2`、B/C 的 `systools:make_relup/4`、A/B/C 的 `systools:make_tar/2` 均有实际调用、路径和成功返回。release node 的 exec、args、env、`peer:start_link` 结果和初始 status 亦已记录。
- 同次 application/nested 日志记录真实 `release_handler:create_RELEASES/2`、`unpack_release/1`、`check_install_release/1`、`install_release/1` 和 `which_releases/0`。fresh direct A→C 的 check/install 均实际返回 `{ok,"1.0.0",[]}`；A→B 与 B→C 返回各自前一 release version，没有 code-path 替代证明。
- 同次 artifact 日志记录每个 `.rel`、`.script`、`.boot`、`relup` 和 tar 的绝对路径、存在性与 SHA-256。关键值：A tar `8C3B0E1BD867BB752850FCC6456CE9CC20E2889656178861F930F7F7019C32B5`；B tar `C20CEF1A71AAD70A52853F551AB7A2304262AC072336109823360B5278EC591A`；C tar `C0A8712F15D9816A78007B6E57F53323E0CD7ED67DFC1A1EF6797A2DC4479AAF`；B `relup` `FFD21FD49AB30F3C800DD0D9A9C65BDA10FF5F5EA4F082CF368BF6C656455503`；C `relup` `BBBB8F5E4E8E7598AB26D9261B48BD6A183B7B58E79B71E4219EF59C4831FDB3`。A 是基线 release，A `relup` 明确记录为 missing，B/C `relup` 被断言存在。
- Owned test paths 中没有 sleep、`code:load_binary/3`、`code:set_path/1`、`code:del_path/1` 或 `code:replace_path/2`。唯一 `code:add_patha/1` 仍只用于独立 node 在 application 启动前加入单一 fixture ebin，不参与 release upgrade 或 upgrade proof。

Findings:

## F1 — completion 通知与 N06/N07 direct-parent checkpoint 仍未确定性同步（blocking）

固定证据：

- `671225b:test/app_fixtures/payment_flow.hrl:57-60` 先向 suite observer 发送 `{payment_complete,...}`，之后才向 payment worker 自身发送状态更新，最后才向 `ParentPid` 发送 `{ParentRef,Outcome}`；`order_flow.hrl:96-99` 同样先通知 observer，后更新 worker completion state。suite 收到 completion 事件并不证明公开 worker state 已完成，更不证明 child outcome 已到达 direct parent。
- `671225b:test/async_sf_migrate_application_SUITE.erl:276-280` 的 N06 在 `complete_payment/2` 后只调用 `assert_runtime/4`；该 helper（`499-507`）只检查版本、tree 与 controller pending，不检查 order callback 已消费 `ParentRef`、中间 completion 或中间 counter map。因此仍可能在 child outcome 尚未发送/消费时释放 order MRef。
- `671225b:test/async_sf_migrate_application_SUITE.erl:288-293` 的 N07 改为先释放 order MRef，但随后的 `assert_runtime/4` 和 mailbox `after 0` 也没有公开证明 order callback 已消费该 MRef；controller reply 与 order worker status 查询来自不同 sender，当前断言不能把实际消费顺序固定下来。

合同依据：

`test.md` N06/N07 要求 child raw/converter outcome 与 parent raw/converter 的实际先后关系，并要求 child result 只经原 `parent_ref` 进入对应 parent；accepted contract 要求 held/complete checkpoint、callback/completion state 与真实事件同步，不能依赖调度偶然性。Review-2 F1 的 observable closure 还明确要求在释放另一 MRef 前通过 public checkpoint 证明目标输入已被 parent 消费，并精确检查该中间点 counters。

修正 owner：

`task-test-3` test correction owner；当前没有产品/runtime finding。

Allowed paths:

首选 `test/async_sf_migrate_application_SUITE.erl`、`test/app_fixtures/order_flow.hrl`、`test/app_fixtures/payment_flow.hrl`；仅在 include wrapper 必须同步时允许对应 A/B/C fixture wrapper，禁止修改产品。

Preserved invariants:

保持 direct `parent_ref`、同名 A/B/C application/module、真实 release 安装、外部 controller、精确 MRef、公开黑盒观察、无 sleep、无私有 callback/plan/bridge 读取、无 code-path upgrade substitute。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

completion 事件只在对应 worker completion state 可公开查询后发出；N06/N07 在释放第二个 MRef 前有确定性的 public consumed checkpoint，精确证明目标 input 已进入正确 direct parent、outer completion 仍为零、非目标状态不变，并对中间和最终 request/action/route/resume/converter、callback、completion、controller counts 做完整等值。

## F2 — N08 已创建 A/B/C pending frame，但仍未证明逐 owner route/raw exact counts（blocking）

固定证据：

- `671225b:test/async_sf_migrate_application_SUITE.erl:302-314` 已分别在 A、B、C current 创建三个可区分 nested pair，关闭了 review-2 的 mixed-ownership 构造缺口。
- `671225b:test/async_sf_migrate_application_SUITE.erl:315-328` 逐步检查 controller pending 与 order/payment completion 数，并对带 sentinel 的 C outcome 做 privacy 检查；最终只调用 `assert_nested_done_count/4`。
- `671225b:test/async_sf_migrate_application_SUITE.erl:558-572` 的 `assert_nested_done_count/4`/`assert_nested_progress/5` 只比较版本、pending、completion 和 controller counts，不比较 order/payment counter map。因此错误 source route、多跑 route/resume/converter，或 C raw frame 意外走 converter，仍可在现有 N08 断言下通过。

合同依据：

`test.md` N08、每阶段断言与 accepted contract 要求 A/B/C mixed pending frame 各按 stored owner 恢复、C current frame 走 raw path、sibling 不串扰，并对 request、route/current step、completion 与 controller 做 exact-once。Review-2 F2 observable closure 明确要求 A/B source route、C raw body及完整 counter closure。

修正 owner：

`task-test-3` test correction owner。

Allowed paths:

仅 `test/async_sf_migrate_application_SUITE.erl`。

Preserved invariants:

保留现有 A/B/C 创建时机、三个可区分 sibling、唯一 privacy sentinel、乱序 reply、逐步 pending/completion、真实 release 安装和 one-shot closure；不得放宽现有断言。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

N08 在关键中间点及最终对 order/payment 完整 counter map 做精确等值：A/B frame 只命中各自预期 source route，C frame 只命中 raw current action/resume，不出现错误 route/converter；request、completion、controller pending/replied 均与三个 pair 精确一致，所有带 sentinel 的公开 outcome 继续证明不泄漏。

## F3 — T14 与 N10 的 exact-counter closure 仍不完整（blocking）

固定证据：

- `671225b:test/async_sf_migrate_application_SUITE.erl:202-210` 的 T14 新增了完整 order counter map，但最终仍依赖 `assert_done/4`；该 helper（`538-546`）只检查 controller `pending = 0`，没有断言 `replied = 1`。因此 review-2 要求的 controller exact count 尚未关闭。T16 在 `216-225` 已增加完整 map和 `#{pending => 0, replied => 1}`，本轮已关闭。
- `671225b:test/async_sf_migrate_application_SUITE.erl:338-348` 的 N10 在 parent-first、child、duplicate 后仍只使用 `assert_runtime/4`、mailbox no-completion 与 `assert_nested_done/3`；最终 helper虽精确检查两个 completion 和 controller `#{pending => 0, replied => 2}`，却从未比较 order/payment 完整 counter map。重复/迟到 child 导致多余 route/resume/converter/current action 时，现有 completion/controller 断言不足以直接拒绝。

合同依据：

`test.md` T14/T16、N10 及 accepted contract 要求 request、stored-owner route、current body/converter、callback、completion 和 controller 全部 exact-once；N10 还明确要求无重复 parent continuation。Review-2 F3 的 observable closure 对 T14 controller replied 和 N10 中间/最终完整 counter map有明确要求。

修正 owner：

`task-test-3` test correction owner。

Allowed paths:

仅 `test/async_sf_migrate_application_SUITE.erl`。

Preserved invariants:

保持 T14 duplicate/unknown 区分、已关闭的 T16 install continuity、N10 parent-first/child-late/duplicate 时序、公开 counters、completion tags 和 controller identity；不得只检查 key presence或放宽预期。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

T14 最终精确断言 controller `#{pending => 0, replied => 1}`；N10 在 parent-first 中间点、child completion 后、duplicate rejection 后分别对 order/payment 完整 request/action/route/resume/converter map、callback pending、completion count及 controller pending/replied 做等值，证明 late/duplicate child 没有第二次 payment body、parent continuation或 outer completion。

Verdict:
`changes_required`

Changes:

仅新增本 review artifact：`docs/plan/sf-migrate/task-test-3-review-3.md`。未修改 candidate 的测试/产品文件，未 stage、commit、merge 或更新 workflow status。

Caveats:

- focused suite 的 `6/6` 绿色证明当前 harness、真实 release 和现有断言可运行，但不关闭 F1-F3 的语义观察缺口。
- 本轮未运行 sibling focused suites、完整 `rebar3 ct`、EUnit、xref、dialyzer 或可选 OTP matrix；已有 blocking semantic findings，新 immutable candidate 仍须绑定合同要求的全部可用 gate。
- focused run 只在 candidate worktree 的 ignored `_build/test` 下生成日志和 release artifacts；运行后 candidate source tree、HEAD 与 tree hash保持精确不变。
- Dispatcher-owned 主 worktree 原有计划文档修改及 review-1/review-2/test.md 未被本审查改动；本轮唯一写入是上述 review-3 artifact。
