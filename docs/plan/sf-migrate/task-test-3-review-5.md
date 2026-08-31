# task-test-3 最终 parent semantic review 5

## Status

`review_complete`

## Task

- Task: `task-test-3`
- Kind: `task-test`
- Owned paths: `test/app_fixtures/**`、`test/test_async_controller.erl`、`test/async_sf_migrate_application_SUITE.erl`
- Review artifact: `docs/plan/sf-migrate/task-test-3-review-5.md`

## Reviewed commit

- Candidate: `308ca7ce86c97759fd6765f2c4c4889dc1914327`
- Tree: `50fb18b836454efcc33a19831e30d2dbcf462247`
- Parent / previous candidate: `92f0b397d1c2929234a962c6a5139fc9b872eae5`
- Contract base: `7552a0c7d6bd1cd167b9822534ad7327352e1f81`
- Branch / HEAD: `codex/sf-migrate-g3-integrate` / `308ca7ce86c97759fd6765f2c4c4889dc1914327`
- `git merge-base` 确认 contract base 是 candidate 祖先。审查开始及全部测试后，HEAD/tree 未变化；工作树仅有审查前已存在的未跟踪 `task-test-3-review-4.md`，没有未提交产品或测试输入混入候选。

## Evidence

### F1/F2/F3/F4 闭合

- **F1 closed：** `order_flow` 为每个 nested request 保存精确 direct `ParentRef` checkpoint；只有 `async_m:run_info/3` 消费对应 `{ParentRef, Outcome}` 且结果不是 `unhandled` 后，才通过 `maps:take/2` 一次性移除并发出 `{parent_consumed, Tag}`。N06/N07 在释放另一 controller MRef 前等待该 checkpoint，并对 order/payment callback pending、completion、controller counts 及完整 route/resume/converter map 做等值断言。
- **F2 closed：** N08 在 A、B、C 三个已真实安装的 application owner 下各创建一个 order/payment pair，以 C/B/A 乱序回复；最终完整 map 精确为 order request/action `3`、A→C route `2`、B→C route `2`、resume `6`，payment request/action `3`、A→C route `1`、B→C route `1`、resume `3`。完整 map 排除了 converter、A→B、replay 或其他历史路径；privacy sentinel 不出现在公开 outcome。
- **F3 closed：** T14 首次回复只完成一次，duplicate 返回 `{error, already_replied}`，order 完整 counter map 不变，controller 精确为 `#{pending => 0, replied => 1}`。N10 对 parent-first、child direct-parent consumption、duplicate rejection 后三个边界分别断言精确 order/payment/controller map，且没有第二次 child 或 outer completion。
- **F4 closed：** correction commit 将 12 个动态 fixture 源做 `R100` 的 `.erl`→`.src` 重命名，源内容未改；suite 用 `epp:parse_file/2` 展开相对 include，再用 `compile:forms/2` 返回的模块名写出 BEAM。`rebar3 eunit` exit `0`，`33 tests, 0 failures`，不再出现 A/B/C 重名 fixture 的 EUnit discovery failure。

### `.src`、application、BEAM 与 release 语义

- A/B/C `.src` 仍分别声明相同四个静态模块名 `sf_fixture_app`、`sf_fixture_sup`、`order_flow`、`payment_flow`，并保留各版本 `-vsn`、`?VSN`、`?MARKER` 及 `-include("../*.hrl")`。本次 focused run 在 CT private data 中实际生成每个版本的上述四个同名 `.beam`。
- 三份 `.app` 都是同一个 `sf_fixture` application，模块表精确为上述四模块，版本/marker 分别为 A `1.0.0/a`、B `2.0.0/b`、C `3.0.0/c`；原 `.app` 与 `.appup` 原样复制进版本 ebin。F4 delta 未改 `.hrl`、`.app`、`.appup`、controller 或既有 F1/F2/F3 断言。
- 动态生成的 BEAM 与 `.app/.appup` 被复制到 CT private `release_roots/{a,b,c}/lib/sf_fixture-{1.0.0,2.0.0,3.0.0}/ebin`；suite 用公开 `systools:make_script/2`、`make_relup/4`、`make_tar/2` 生成 `.rel`、`.script`、`.boot`、`relup` 和 tar。
- 每个 release scenario 从 A boot 启动专用 node；每次升级都实际调用并断言 `release_handler:unpack_release/1`、`check_install_release/1`、`install_release/1`，随后检查 `which_releases/0` current/permanent 状态及 fixture marker。T/N matrix 合计执行 35 次真实 install，覆盖 A→B、B→C、A→B→C 和 fresh A→C；没有以 module load 或 code-path replacement 代替 release upgrade。
- install 前后精确比较 application master、完整 supervisor/worker tree、外部 controller pid/count/pending 和每个 `{Pid,MRef}` identity；A/B/C 独立 node 也分别启动同名 application/tree，并证明 controller/application/tree 相互独立。

### T01-T18 / N01-N10、controller、隐私与禁止路径

- `application_scenario_matrix/1` 可见一一映射并执行 T01-T18；`nested_scenario_matrix/1` 可见一一映射并执行 N01-N10。focused suite 的两个 matrix case 均通过，因此 28 行没有 omission、skip 或 alternate path。
- `test_async_controller` 只保存由 `async_gen_server:call/2` 收到的精确 `{Pid,MRef}`；reply 只经 `gen_server:reply/2`，`maps:take/2` 删除 live entry，replied map 保留 pid 以区分 `{error,already_replied}` 与 `{error,unknown_mref}`。unknown/duplicate 都不改变错误路径以外的状态，任意顺序及 identity continuity 由 suite 断言。
- 公开观察限于 application/version/marker、tree pid、controller identity/count、worker pending/completion/counter 和有界 error；未读取 callback record、Env、plan、generated bridge 或 runtime 私有结构。`contains_private/1` 与 N08 精确 sentinel 断言覆盖公开 outcome 泄漏。
- `rg` 对 owned paths 搜索 `timer:sleep`、非零 receive-after 排序、`code:load_binary`、`code:set_path`、`code:del_path`、`code:replace_path` 无匹配。唯一 `code:add_patha/1` 只在独立 baseline node 启动 application 前加入单一 fixture ebin，不参与 release install 或升级证明。
- 从计划提交 `2e069859...` 到 candidate 的测试实现变更只落在三个 owned path 集合；F4 correction commit 本身只改 owned fixture/suite 文件，没有产品、配置、既有测试、workflow status 或 `test.md` 改动。

### Commands

- Runtime: Erlang/OTP `29`，ERTS `17.0.4`，rebar3 `3.25.1`。
- `rebar3 eunit`：exit `0`，`33 tests, 0 failures`。
- `rebar3 ct --suite=test/async_sf_migrate_application_SUITE.erl`：exit `0`，`6/6` passed，`0` skipped。日志：`_build/test/logs/ct_run.nonode@nohost.2026-08-31_21.00.51/lib.async.async_sf_migrate_application_SUITE.logs/run.2026-08-31_21.00.52/suite.log`；release/fixture artifacts 位于同目录 `log_private/`。
- `rebar3 ct --suite=test/async_sf_migrate_SUITE.erl,test/async_sf_migrate_nested_SUITE.erl`：exit `0`，`16/16` passed。
- `rebar3 ct`：exit `0`，`99/99` passed；其中 application/release suite 再次 `6/6` 通过，完整 run 日志根：`_build/test/logs/ct_run.nonode@nohost.2026-08-31_21.01.57/`。
- `rebar3 xref`：exit `0`。
- `rebar3 dialyzer`：exit `0`。
- `git diff --check 7552a0c7d6bd1cd167b9822534ad7327352e1f81..308ca7ce86c97759fd6765f2c4c4889dc1914327`：exit `0`。

## Findings

None. review-3 的 F1/F2/F3 与 review-4 的 F4 均已在同一 immutable candidate 上闭合，没有 open test-owned 或 product-owned finding。

## Verdict

`passed`

Candidate `308ca7ce86c97759fd6765f2c4c4889dc1914327` 满足 `task-test-3` 的 observable completion 和全部当前可用 blocking gates；parent semantic review 建议 merge-ready。

## Changes

仅新增本 review artifact。未修改源代码、测试代码、`test.md` 或 workflow status；未 stage、commit、merge。

## Caveats

- 可选 OTP 21–29 Docker matrix 本轮未运行；合同将其定义为环境可用时的可选验证，不影响本次 OTP 29 真实 release 证据与 `passed` verdict。
- 审查前已存在的未跟踪 `task-test-3-review-4.md` 保持原样；它不属于 candidate tree，也未参与编译或测试。
