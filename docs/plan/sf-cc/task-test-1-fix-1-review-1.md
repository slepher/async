# Derived Coding Fix Review

Status: complete

Task: task-test-1-fix-1

Reviewed commit: aab6356893524e0cb36293837a4c995d6a33769e

Previous reviewed commit: bc28febe142000025abaf59c71c8b41ab6cffe99

Evidence:

- immutable candidate `aab6356893524e0cb36293837a4c995d6a33769e` 的唯一 parent 与 fix base 均为 `bc28febe142000025abaf59c71c8b41ab6cffe99`；diff 仅修改 contract-owned `src/async_m.erl`，`git diff --check` 通过。
- candidate 从 generated facade 列表移除 `handle_info/3` 并增加同名显式 public adapter；`run_info/3` 仍由既有 generated adapter直接委托 `async_t:run_info/4` 并执行 `identity:run`，没有新增 runtime execution path。
- R15 tagged path：`async_t:run_info/4` 的既有 unavailable-old-callback branch 产生 `{{legacy_outcome, Outcome, CleanState}, CleanState}`；新的 `async_m:handle_info/3` 精确匹配该 pair 并原样返回内层 `{legacy_outcome, Outcome, CleanState}`，没有投影、重建或二次清理 State。
- ordinary path：所有非 R15 pair 继续返回第二元素 `NewState`，与此前 generated `async_t:handle_info/4` 对 `{Outcome, NewState}` 的 state projection 相同；message/reply/timeout/DOWN 的普通 public result shape 未改变。
- ownership audit：candidate 只在 public adapter 识别已经规范化的 `legacy_outcome` representation；unavailable classification、business exception/nested `undef` distinction、old-tuple cleanup 与 consumed/exactly-once owner 仍唯一位于 `async_t`。没有复制 classifier、callback decoder、cleanup、CcRef owner或 legacy state normalization。
- supplied worker evidence：public probe 返回精确 tagged value；resumable old tuple、duplicate event、business exception、nested `undef` probes 通过；compile、EUnit、xref 通过。这些 evidence 与单路径 delta 一致，未报告新 failure/signature 或越界修改。

Findings:

- none。

Verdict: passed

Changes:

- none required。derived coding fix 可进入 dispatcher 的 checkpoint persistence、merge 与 task-test-1 resume gate；本 review 不自行 stage、commit、merge或重跑 integration task。

Caveats:

- task-test-1 full rerun 尚未由本 review 执行或声明通过；必须在 fix merge 后重新运行，并生成新的 task-test report/review closure。
- dialyzer、OpenSpec validation 与 OTP matrix 未运行，亦未声明通过。
- 本 review 未修改产品、test、contract、status 或 Git state，只写入本 immutable review artifact；未 stage、commit，也未修改 plan、lanes、ledger、OpenSpec 或 checklist。
