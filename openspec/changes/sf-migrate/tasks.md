# Tasks

当前 master 只把已完成的 `sf-do` 作为依赖基线；旧 sf-migrate 产品实现已经 revert，其他
worktree/branch 的实现仅可作为历史证据，不得据此勾选本文件。每个 task 是可独立实现、
测试、review 和 commit 的纵向公开行为切片，不按 parser/manifest/runtime 内部阶段拆分。

## Dependency gate

- [ ] `sf-do` 的 identity 协议已实现并冻结：`sf_do/1,2` 使用 inline atom marker，named identity
  只使用 Protocol + BlockPath + StepName；unnamed frame warning、普通执行可用、migration failure；
  Action/index/continuation/capture 不参与 identity，相同 named AsyncId/MFA 默认自动继承但允许显式 Env-only
  migration，不同 AsyncId 默认 directional `incompatible_frame`。
- [x] 当前 master 不含 sf-migrate 产品实现；开始 Task 1 前只运行 dependency baseline，
  不把 baseline 或最终命令验证建立为独立产品 task。

## Task 1 — 可用的 upgrade migration

**目标：** 用户可声明完整 historical block，把 pending historical frame 显式迁移或
current-refresh 到 current stable function，并获得最终 abort-only result/error contract。

**依赖：** dependency gate。Task 1 不支持 down execution 或 State-aware context；在后续
task 完成前，相关字段必须被明确拒绝，不能静默接受后失效。

- [ ] 在 `async_sf_transform` 中实现 inline-marker `sf_migrate/2` declaration、
  explicit/anonymous-path transition、exact version anchors、StepName selectors 和 current/historical
  cross-validation；historical block 只生成 metadata。
- [ ] 只支持 `sf_migrate([Monad || ...], Spec)` historical block，复用 sf-do 的 inline marker
  parser/warnings，并只实现 `{step,StepName}` selector；unnamed historical boundary 不可映射。
- [ ] 采用最终 directional format-1 manifest schema并原子更新 compiler/runtime；Task 1
  生成 `plans.up` 和空 `plans.down`，只接受 `directions.up`。不兼容旧 flat draft，也不
  增加 format/protocol 版本。
- [ ] 只有存在 declaration 时生成/export migration manifest；用户同名 function 必须得到
  compile-time collision diagnostic。
- [ ] 保持 `async_m:migrate_sf_callbacks/4` 为薄 facade，在独立 `async_sf_migration` 中实现
  manifest/target-state/frame preflight、historical mapping、same-ABI reuse、runtime Env
  mismatch、current-anchor refresh 和一次 target callbacks setter commit。
- [ ] 相同 BlockName/AsyncId/MFA 默认 automatic inheritance；若 resolved source version 存在
  directional `migrate_env`，允许 same-identity Env-only plan 先生成 TargetEnv并保持 identity/MFA。
  不同 AsyncId 没有 plan 时不得 fuzzy match。
- [ ] current-anchor 必须在 `steps => []` 或无 historical mapping 时独立工作；upgrade source
  未锚定时 entry 原样 unhandled。
- [ ] terminal policy 只允许 abort；删除 replacement/replaced 路径。实现固定
  source/target result fields、structured error stage/reason、入口 TargetState + 空 error
  Results、精确 `{error,{async_aborted,{sf_migration,Info}}}` notification wire format。
- [ ] ABI 不同且 up migrator 缺失时允许编译，并在实际 frame 上进入 runtime mismatch；
  migrator failure/local-fun result 使用同一 policy path。
- [ ] 复用既有 State merge、stable-term、completion 和 map lookup；不得把 orchestration
  并入 `async_sf_runtime` 或复制 helper。
- [ ] 添加 declaration/anchor/selector/current-refresh、manifest collision/conditional export、
  same-ABI/migrator、abort/forced-abort、notification failure、deterministic order、mixed
  callbacks、pending request continuity、purge 和 structured error/result behavior tests。
- [ ] 添加 BlockPath/StepName rename、相同 named step automatic inheritance、Action/index changes
  不影响 identity、same-identity capture rename/同名 value 转换，以及缺少显式 mapping 默认失败的 tests。
- [ ] 同步 README：upgrade declaration/options、current refresh、result/error 与排错。

**独立验收：** 仅使用 `directions.up` 的应用能完成真实 pending-frame upgrade；Task 1
commit 不包含半成品 down frame、State context 或 replacement compatibility seam。

## Task 2 — StepName-only selector surface

**目标：** migration 只通过 source 显式 StepName 定位 historical/current boundary，不维护第二套
index/content/name matching 机制。

**依赖：** Task 1。该 task 不改变 runtime migration protocol，可与 Task 3 的设计并行，
但若修改同一 transform 文件必须串行集成。

- [ ] historical/current block 仅建立 StepName → AsyncId table；old/new selector 分别精确解析。
- [ ] index、bind name、content 和 Offset selector 均在编译期作为 unsupported syntax 拒绝。
- [ ] manifest/runtime 只消费 resolved AsyncId，不保存或重算 selector AST。
- [ ] 添加 StepName found/missing/duplicate/rename 与所有 fuzzy selector rejection tests，并同步
  README selector 表。

**独立验收：** 所有 selector 错误均在编译期确定，Action/index 变化不会改变同名 step 的解析。

## Task 3 — OTP-ordered downgrade migration

**目标：** v4 current module 在 OTP 加载 v3 前，把 pending current frame 原子迁移为可由
v3 接管的 historical frame，或按显式 policy abort；失败阻止 code rollback。

**依赖：** Task 1。Task 2 非必需；本 task 使用 `{step,StepName}` selectors。

- [ ] 固定 old=historical/new=current；启用 `directions.down`，up/down 使用独立 plan 与
  Env migrator，绝不推断逆函数。
- [ ] historical metadata 保存 target runtime BlockName/AsyncId/MFA/CaptureAbi；down plan 以
  NewAsyncId 为 source。只构造 historical frame，不生成或提前调用 historical function。
- [ ] historical target 的 BlockName/AsyncId/MFA 与 source 相同时默认 automatic inheritance；存在
  explicit down Env migrator 时执行 same-identity Env-only plan，不同时才要求 boundary mapping 或
  terminal policy。
- [ ] 实现 `{down,TargetVsn}` 方向、historical target anchor、current source validation 和
  target historical State layout/offset；应用先完成业务 State downgrade并保留 callbacks。
- [ ] 在 mutation 前预检所有 source frames、down plans、historical targets 和 target offset；
  missing migrator 是 runtime mismatch，missing plan/target 无 policy 时是 structured error。
- [ ] downgrade 默认不得 implicit abort。只有显式 policy abort 后才可继续；成功后不得残留
  current/new stable frame。
- [ ] runtime migration failure 返回入口 TargetState与空 Results；timer/monitor cancellation、
  notification 和 frame commit 延迟到整个调用可提交。记录用户 callback 外部副作用不在
  rollback 保证内并要求其 side-effect-free。
- [ ] 添加真实 OTP-order v4→v3 integration：v4 `code_change({down,V3},...)`、v3 target offset、
  old MFA 延迟执行、unknown target、missing plan、invalid historical target、explicit abort、
  多 entry late failure 原子性和 anonymous↔named transition。
- [ ] 同步 README：appup/downgrade 顺序、target State/offset、directional migrators 和错误处理。

**独立验收：** 一个没有 State-aware capture 的应用可真实 downgrade；任一不可处理 entry
均在旧 module load 前失败，且 runtime 管理的 State/frame/lifecycle side effects 为零。

## Task 4 — Directional State-aware Env context

**目标：** Env migrator 可显式读取该方向 source frame 捕获的 State和入口 target-layout
State snapshot，不由 runtime 猜测或自动转换 State。

**依赖：** Task 3。

- [ ] 实现 declaration/step-level `state`、`up_state`、`down_state`；shorthand、继承、重复和
  conflict 规则与最终 contract 一致，不按 version 分支、不跨 declaration 传播。
- [ ] up_state 在 historical source layout、down_state 在 current source layout 编译期验证；
  没有该方向 State capture 时 context 精确为 `#{env=>SourceEnv}`。
- [ ] 有 effective capture 时提供 `source_state` 与固定 `current_state` snapshot；同次调用的
  所有 migrator 看到 API 第四参数入口原值。
- [ ] runtime 不自动 appup/appdown State，不使用 working/policy State替代 snapshot，也不把
  current_state 隐式写入 TargetEnv；缺 key、异常返回和 unstable TargetEnv 进入 mismatch。
- [ ] 添加 shorthand、不同 directional names、declaration inheritance/repeat/conflict、
  source-layout validation、up/down contexts、multi-entry snapshot、缺 key 和不隐式写回测试。
- [ ] 同步 README：State fields、source_state/current_state 方向语义和应用转换职责。

**独立验收：** up/down State-aware migration 均通过公开 context 行为测试；关闭 State
directive 时 Task 1/3 的 context 与行为 bit-for-bit 不变。

## Optional follow-up — fun-valued Env regeneration

本节不属于当前 initiative final acceptance gate。

- [ ] 可选：允许 explicit directional Env migrator 返回由 target code 新生成的 anonymous/local
  fun，同时保持 same-identity/boundary-mapping 的既有 identity 与 callback lifecycle 规则。
- [ ] 可选：记录并测试 old fun 不会在 State 中自动清除、source body 相同不构成兼容、old code
  purge 后不得调用旧 fun，以及每次相关 upgrade/downgrade 都重新生成 target-version fun。
- [ ] 未实现该能力时，base runtime 可以 `unstable_capture` 拒绝 local-fun TargetEnv；用户文档应
  推荐 external MFA/data descriptor。

## Initiative final acceptance

以下是 initiative 完成门，不建立 command-only task：

- [ ] `proposal.md`、`design.md`、`implementation.md`、spec、tasks 和 README 的 API、
  result/error、abort wire format、manifest schema 与完成状态一致。
- [ ] direct/local 历史实现中只移植公开行为测试，不引入私有 helper/结构组合矩阵。
- [ ] `rebar3 compile`、EUnit、Common Test、xref 和 migration integration suites 通过。
- [ ] `rebar3 dialyzer` 不新增 sf-migrate warning；不得用宽泛类型或 suppress 隐藏问题。
- [ ] `openspec validate sf-migrate --strict` 通过。
- [ ] 所有最终 artifacts staged 后运行 `git diff --cached --check` 并通过。
