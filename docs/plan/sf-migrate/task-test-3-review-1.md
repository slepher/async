# task-test-3 Parent Semantic Review 1

Status:
review_complete

Task:
`task-test-3`（`task-test` parent semantic review）

Reviewed commit:

- Candidate: `8057871c049879fa53f89e73149f7cf4f55130ae`
- Tree: `06aa2500e3ba37f7382bffaa21b3efd1a5820054`
- Branch: `codex/sf-migrate-g3-integrate`
- Worktree: `/home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate`
- Accepted base: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- Integrate start / candidate parent: `2e06985948c185ee60c0f2afd7d4f03fa031d17b`
- `HEAD` 与 branch ref 均精确解析为 candidate；accepted base 是 candidate 祖先。`2e069859..8057871` 仅新增合同拥有的 24 个文件，未发现 staged、unstaged 或 untracked 候选内容。

Evidence:

- 范围：candidate commit 恰好新增 24 个 owned files：`test/app_fixtures/**` 下 22 个文件、`test/test_async_controller.erl`、`test/async_sf_migrate_application_SUITE.erl`；无 `src/`、既有测试、配置或文档变更。
- A/B/C：三个目录均声明 application `sf_fixture`，并使用同名模块 `sf_fixture_app`、`sf_fixture_sup`、`order_flow`、`payment_flow`；`.app` 版本分别为 `1.0.0`、`2.0.0`、`3.0.0`，注册树为 `sf_fixture_sup -> order_sup -> order_worker` 与 `sf_fixture_sup -> payment_sup -> payment_worker`。
- Controller：`test_async_controller` 保存精确 `{Pid,MRef}`，通过 `gen_server:reply/2` 一次性删除 live entry，并区分 `{error,already_replied}` 与 `{error,unknown_mref}`；独立 controller case 在当前 candidate 通过。
- 禁用路径：candidate 中无 `code:load_binary/3`、无 sleep；release 场景未改 code path。`start_fixture_node/2` 仅在独立节点启动 application 前通过 `peer:call(P, code, add_patha, [Ebin])` 放入单一 fixture ebin，这属于 `test.md` 指定的启动时路径隔离，不是升级证明或 code path swap。
- 真实 release：`systools:make_script/2`、`systools:make_relup/4`、`systools:make_tar/2` 与 `release_handler:unpack_release/1`、`check_install_release/1`、`install_release/1`、`which_releases/0` 均实际执行。生成的 C `relup` 同时包含从 `1.0.0` 和 `2.0.0` 到 `3.0.0` 的直接 entry；focused log 记录 direct A→C 的 `check_install_release("3.0.0") -> {ok,"1.0.0",[]}` 与 `install_release("3.0.0") -> {ok,"1.0.0",[]}`。
- 运行环境：OTP `29`，ERTS `17.0.4`，rebar3 `3.25.1`。
- `rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`：exit `0`；`6 ok, 0 failed, 0 user_skipped, 0 auto_skipped`。日志：`_build/test/logs/ct_run.nonode@nohost.2026-08-31_19.16.35/lib.async.async_sf_migrate_application_SUITE.logs/run.2026-08-31_19.16.35/suite.log`。其中 `application_scenario_matrix` 与 `nested_scenario_matrix` 各是一个 CT wrapper case，内部顺序执行 T01-T18 与 N01-N10。
- `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl,test/async_sf_migrate_nested_SUITE.erl`：exit `0`；`16 ok, 0 failed, 0 skipped`。日志根：`_build/test/logs/ct_run.nonode@nohost.2026-08-31_19.17.06/`。
- `git diff --check 2e06985948c185ee60c0f2afd7d4f03fa031d17b..8057871c049879fa53f89e73149f7cf4f55130ae`：exit `0`，无输出。
- 本轮生成产物的关键 SHA-256：A tar `ac1ec3ddddd21015547c12cd6a2506c43f6ee9965eae6949a58dda8268fbf73e`；B tar `3989ffd6b9cc94d84f2888085324e9b04521927d517eb41494481bdb5b792ffe`；C tar `98de45557b324ddc17b5f87594f14758970877b4d0ff8fa97103f9e4c6f19364`；B `relup` `ffd21fd49ab30f3c800dd0d9a9c65bda10ff5f5ea4f082cf368bf6c656455503`；C `relup` `bbbb8f5e4e8e7598ab26d9261b48bd6a183b7b58e79b71e4219ef59c4831fdb3`。产物根：`_build/test/logs/ct_run.nonode@nohost.2026-08-31_19.16.35/lib.async.async_sf_migrate_application_SUITE.logs/run.2026-08-31_19.16.35/log_private/release_roots/`。
- 首次 N09 失败日志（仅用于失败归属，不作为 candidate 通过证据）：`_build/test/logs/ct_run.nonode@nohost.2026-08-31_19.04.36/lib.async.async_sf_migrate_application_SUITE.logs/run.2026-08-31_19.04.36/suite.log`，结果为 `5 ok, 1 failed`，失败值是 `{unexpected_order_error,{ok,{order,{3,0,0},n09_parent,{resume,order}}}}`。candidate 的 `test/app_fixtures/order_flow.hrl:70-80` 已包含 raw `{error,Reason}` direct-parent 分支，当前 immutable candidate 重跑为 `6/6`。该失败归属测试 fixture，不是产品/runtime。

Findings:

## F1 — N02/N06/N07/N10 的事件序列没有实现各自标签语义（blocking）

Immutable evidence:

- `8057871:test/async_sf_migrate_application_SUITE.erl:213-217` 在任何 child reply 前先 A→B，随后断言 payment 在 `{2,0,0}` 完成；这不是 N02 要求的 “payment 仍由 A 完成”。
- `8057871:test/async_sf_migrate_application_SUITE.erl:234-238` 在 child 与 parent 都 pending 时先 A→B，再回复两者，因此 N06 实际同时走 parent converter 与 child converter，不是 `parent converter、child raw`。
- `8057871:test/async_sf_migrate_application_SUITE.erl:239-243` 在 parent 与 child 都 pending 时先 A→B→C，再回复两者，因此 N07 实际同时走 converter，不是 `parent raw、child converter`。
- `8057871:test/async_sf_migrate_application_SUITE.erl:259-264` 先回复 child、后回复 parent；这没有制造 late child。重复动作只被 controller 以 `{error,already_replied}` 拒绝，`assert_no_completion(n10)` 也不会匹配 payment 使用的 tag `{child,n10}`。

Contract basis:

`test.md` N02、N06、N07、N10 及 accepted contract “原始值/转换器组合、迟到/重复子级处理、严格的一次外层完成”；ID 必须对应真实事件，不得只是 scenario label。

Correction owner:

`task-test-3` test correction owner；当前无产品/runtime 路由。

Allowed paths:

首选仅 `test/async_sf_migrate_application_SUITE.erl`；只有现有公开观察不足时，才允许最小修改 `test/app_fixtures/order_flow.hrl`、`test/app_fixtures/payment_flow.hrl` 及对应 A/B/C wrapper。

Preserved invariants:

保持同名 A/B/C application/module、真实 release 安装、controller 位于 fixture tree 外、精确 MRef、direct parent_ref、无 sleep、无 `code:load_binary/3`、无 code path upgrade substitute。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

日志和断言分别显示 N02 child 在 A 完成而 parent A frame 在 B 恢复；N06 仅 parent route counter 增加；N07 仅 child route counter 增加；N10 在 parent 已回复但仍等待 child 时不完成 outer，late child 仅唤醒一次，重复 child 不产生第二个 `{payment_complete,{child,n10},_}`、parent continuation 或 outer completion。

## F2 — T11-T18 与 direct A→C 的断言不足以证明逐帧 owner、counter 和 exact-once（blocking）

Immutable evidence:

- `8057871:test/async_sf_migrate_application_SUITE.erl:157-177` 的 T11-T13 最终只检查 A→C route 是否存在；没有分别证明 B frame 使用 B→C、C frame 使用 raw current path，也没有检查每个 route/body/completion 的精确次数。
- `8057871:test/async_sf_migrate_application_SUITE.erl:194-198` 的 T17 在两个 completion 后没有最终 callback/controller/counter closure assertion。
- `8057871:test/async_sf_migrate_application_SUITE.erl:199-206` 的 T18 只检查 A→C route 存在，没有证明 A/B/C 三个 frame 各按 stored Vsn 路由。
- `8057871:test/async_sf_migrate_application_SUITE.erl:437-443` 使用 `completion_count >= 1` 和 `maps:is_key/2`；重复 completion、route/body 多跑、错误 route 与 raw request count 不为一仍可能通过。T06 也未显式断言 B route/body counter 为零。

Contract basis:

`test.md` T06-T07、T11-T18，以及 accepted contract “原始请求计数为一；任何历史主体/转换器都不会提前运行；只有回复时选中的路由/当前步骤递增；每个帧所存版本的所有权；严格一次完成”。

Correction owner:

`task-test-3` test correction owner；当前无产品/runtime 路由。

Allowed paths:

`test/async_sf_migrate_application_SUITE.erl`；若现有 counters 不能表达 exact count，可最小修改 `test/app_fixtures/order_flow.hrl` 与 B/C `order_flow.erl`，不得修改产品。

Preserved invariants:

保持现有真实 T event 顺序、A→B→C 与 fresh A→C release topology、任意回复顺序、controller duplicate/unknown 语义、公开观察边界。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

每个 T row 都对 expected request count、stored owner route、current body/step count、completion count、callback pending、controller pending/replied 给出精确等值；T06/T07 明确证明 B route/converter/body 为零；T11/T12/T17/T18 分别证明 A/B/C frame 的独立 owner 和 exact-once closure。

## F3 — upgrade continuity 的 “before/after” snapshot 实际都是升级后读取（blocking）

Immutable evidence:

- `8057871:test/async_sf_migrate_application_SUITE.erl:424-434` 在 `upgrade/2` 已完成后才调用 `observe(BeforeN)` 与 `observe(AfterN)`。两个 map 指向同一个 live peer/controller，差别仅是本地 `current` 字段，因此 tree/controller count equality 是 post-upgrade snapshot 与自身比较。
- `8057871:test/async_sf_migrate_application_SUITE.erl:631` 的 `BeforeController` 也直接取自同一个未改变的 node map，再与该 map 自身比较。
- 升级前后的 `identity/2` 断言都使用 `{pending,{_,MRef}}`，未保存并比较精确 `{Pid,MRef}`。

Contract basis:

accepted contract 与 `test.md` 要求每次安装前后证明 controller pid/state、精确 saved From、pending MRef、application availability、完整 tree pid 连续且可查询。

Correction owner:

`task-test-3` test correction owner；当前无产品/runtime 路由。

Allowed paths:

仅 `test/async_sf_migrate_application_SUITE.erl`。

Preserved invariants:

保持 `release_handler` 安装流程、同一 controller、同一 worker/supervisor tree、不引入 sleep 或私有 runtime state reader。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

每次 `install_release` 前先捕获 immutable public snapshot，安装后再捕获 snapshot，并精确比较 application pid、五个 tree pid、controller pid、controller counts、pending MRef 列表和每个 MRef 的 `{Pid,MRef}`；同时证明升级后 application/tree 仍可查询。

## F4 — nested parent_ref、sibling isolation、privacy 与 child one-shot 仍缺少黑盒闭环（blocking）

Immutable evidence:

- `nested_request/2` 只断言 `ParentRef` 是 reference；N08（`8057871:test/async_sf_migrate_application_SUITE.erl:244-250`）只有一个 nested parent/child，加一个普通 order request，没有两个 parent_ref/sibling child 可用于证明 child 只进入自己的 direct parent。
- N08 在各次 reply 间没有断言非目标 callback/completion 保持不变，提前或错误消费可被后续宽松 completion receive 掩盖。
- `8057871:test/async_sf_migrate_application_SUITE.erl:480-485` 的 privacy 检查只搜索固定 atom `[env,outcome,state,callback,'fun',secret,sentinel]`；测试没有注入并验证唯一敏感 sentinel，任意不含这些 atom 名称的 Env/State 值泄漏仍会通过。
- N10 的 payment tag 是 `{child,n10}`，而 `assert_no_completion(n10)` 不会捕获重复 `{payment_complete,{child,n10},_}`。

Contract basis:

`test.md` N08-N10 与每阶段失败断言；accepted contract 要求 direct `parent_ref`、sibling isolation、无 Env/内部信息泄漏、迟到/重复 child one-shot、outer 严格完成一次。

Correction owner:

`task-test-3` test correction owner；若加强后的公开复现显示产品错误，再将最小复现和精确 release/node evidence 路由给 covered coding owner；当前证据不足以认定产品缺陷。

Allowed paths:

首选 `test/async_sf_migrate_application_SUITE.erl`；仅在缺少必要公开观察时允许最小修改 `test/app_fixtures/order_flow.hrl`、`test/app_fixtures/payment_flow.hrl`。

Preserved invariants:

不读取 callback record、plan、AST 或生成 bridge 名称；不把 Env/State 暴露为测试接缝；保持 child result 只经公开消息和 direct parent_ref 路由。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`

Observable closure:

至少两个可区分 nested parent/child pair 乱序回复，每一步证明 sibling pending/completion 不变；结果中验证唯一 privacy sentinel 不出现；N09 bounded error 只到对应 parent；N10 对 `{child,n10}` 与 outer tag 分别证明 late/duplicate 后 completion count 仍为一。

## F5 — required release/node evidence 没有由 suite 完整捕获（blocking）

Immutable evidence:

- `8057871:test/async_sf_migrate_application_SUITE.erl:112-119` 只检查 rel/script/boot/tar 存在并打印 metadata，且只检查 C `relup`；没有产物 hash、B `relup` 内容/路径 closure。
- `8057871:test/async_sf_migrate_application_SUITE.erl:535-550` 以 `silent` 调用 `systools`，随后 `systools_ok/1` 丢弃成功结果；日志没有每个 `make_script`、`make_relup`、`make_tar` 调用与结果。
- `8057871:test/async_sf_migrate_application_SUITE.erl:621-636` 打印三个 install API 结果，但不打印 `which_releases`、安装前后 public snapshots、node exec/args/status；suite 自身也未记录 rebar3 版本。

Contract basis:

accepted contract “为审查/完整测试证据捕获”与“必需证据”明确要求工具/运行时版本、每个生成/安装调用及结果、产物路径和 hash、节点命令/日志/状态、`which_releases`、每次安装前后 controller/tree/pending snapshot。

Correction owner:

`task-test-3` test correction owner；full_tester 负责在新 immutable candidate 上保存并交付日志。当前无产品/runtime 路由。

Allowed paths:

仅 `test/async_sf_migrate_application_SUITE.erl`；运行日志与生成产物仍限 Common Test private data 或 `_build/test`。

Preserved invariants:

继续使用 OTP stdlib/public API，不增加依赖、仓库 release 配置、私有 release state reader 或源目录产物。

Focused validation:

`rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`，并对该次 CT private data 下的 `.rel`、boot、relup、tar 做 hash/路径核对。

Observable closure:

同一 immutable candidate 的日志可直接恢复 OTP/ERTS/rebar3、peer exec/args/node、每个 `systools` 与 `release_handler` 调用/返回、`which_releases`、安装前后 snapshots、A/B/C metadata、全部 rel/script/boot/relup/tar 路径及 SHA-256；无 hidden skip。

Verdict:
`changes_required`

Changes:

仅新增本 review artifact：`docs/plan/sf-migrate/task-test-3-review-1.md`。未修改、stage、commit 或 merge candidate 的测试/产品文件，也未更新 workflow state。

Caveats:

- 当前 focused suite 绿色只证明现有断言通过，不关闭上述语义覆盖缺口。
- T01-T18 与 N01-N10 作为一一对应 scenario table 可见，wrapper case 的 `1/1` 计数本身不是 finding；修正后仍应保留可从 ID/stack/log 精确归属失败的能力。
- 本轮未运行全量 `rebar3 ct`、`rebar3 eunit`、`rebar3 xref`、`rebar3 dialyzer` 或可选 `rebar3 docker_ci`；已有 blocking semantic findings，修正后的新 immutable candidate 仍需执行合同全部可用关卡。
- focused test 仅在 ignored `_build/test` 下生成运行日志与 release 产物；source candidate 保持精确不变。
