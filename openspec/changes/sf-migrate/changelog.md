# Changelog

## 2026-08-28 — linked continuation migration contract（机制同步，未实现）

- migration identity同步为显式`{Protocol, Block, StepName}`；plan、manifest与result按Block/StepName直接
  索引，不保存或交换AsyncId/hash。相同identity/MFA默认继承，不同Block/Step必须显式mapping。
- migration 的持久化对象从 callback 内递归 continuation chain 收敛为单个完整 stable node：
  `#sf_callback{frame = CurrentFrame, acc_ref = AccRef, parent_ref = ParentRef,
  timer_ref = TimerRef}`。`frame + parent_ref` 只是
  continuation 投影，不是完整 record。
- eager migration 扫描 callbacks store 中所有 stable nodes，并逐 entry 迁移其当前 frame；不得递归改写
  某个 callback 内的 outer chain，也不得依赖每层携带 final/root CcRef。
- reply-time lazy migration 只解析当前节点。成功后按 `parent_ref` 继续；failure/abort 作为 Outcome 只转发
  给直接外层节点，由外层继续传播或处理，不再直接 terminal-complete root CcRef。
- downgrade preflight 必须覆盖本次将提交的全部 stable nodes 和引用边；commit 只替换对应 node.frame，
  保留 entry key、`parent_ref`、request/timer/monitor/accumulator lifecycle 数据。
- ordinary `#callback{cc,acc_ref,parent_ref,...}` 继续保持隔离；统一的是 continuation reference，不是把
  anonymous ordinary callback误分类为可迁移 frame。
- 本条 supersede 旧文档中所有“通过 callback root CcRef terminal-abort”以及“递归 chain migration”含义；
  StepName/Block identity、directional Env/Input conversion 和 OTP downgrade ordering 不变。

## 2026-08-24

- 收敛 `sf-do` dependency 为 inline StepName marker：使用 `sf_do(Comprehension)` 与
  `sf_do(BlockName, Comprehension)`；named identity 只使用 Protocol、Block 和 StepName，
  unnamed frame warning 且 migration failure。删除 derived identity、Action canonicalizer/AsyncForm
  digest 依赖。
- historical declaration 同步收敛为 `sf_migrate(OldDoBlock, Spec)`，StepName 内联在 historical
  qualifiers 中；mapping 只接受 `{step,StepName}`，unnamed boundary 不可映射，也不再支持 index、
  bind name、content 或 Offset fuzzy selectors。本文以下
  2026-08-21 的 derived/adoption 记录仅保留为被本条取代的历史草案。
- frame Env 与 CaptureAbi 收敛为 capture-name atom map及排序后的 atom list，不为 capture key/layout
  维护 SHA、binder-origin identity 或第二套 canonicalizer。
- 修正 Env migration 边界：相同Protocol/Block/StepName/MFA仍默认automatic inheritance，但允许显式配置
  directional `migrate_env` 的 same-identity Env-only mapping，用于变量改名、capture set 变化或
  同名变量 value 语义转换；该 path 只替换 Env，不改变boundary identity/MFA。
- 因此 2026-08-21 中“`sf-migrate` 只处理不同boundary identity”的表述被本条取代；无 migrator 的
  same-identity mapping 仍报 `redundant_sf_migration_mapping`。
- 增加 fun-valued Env 责任说明：upgrade 不会自动清除或重写 State 中的 anonymous/local fun，
  source body 未变化也不表示旧 fun 已指向新 code。原则上避免捕获；需要时由 explicit Env migrator
  在 target code 中重新生成。该 regeneration support 记为 optional follow-up，不进入当前验收。

## 2026-08-21

- 增加可选 stable step naming：`sf_do` 三参数 template 使用
  `sf_do(BlockName, StepNames, [Monad || ...])`，StepNames 可为 record 名或 atom list；一、二参数
  形式继续支持 derived identity，不要求用户初始版本命名。
- named frame 使用 BlockName + StepName identity；index/form 只作诊断。`sf-migrate`
  增加 named historical `sf_migrate(StepNames, [Monad || ...], Spec)` 与
  `{step,StepName}` selector，用于首次
  derived → named、named → derived 或 step rename；相同 named identity 仍自动继承。
- 划清 `sf-do` automatic inheritance 与 `sf-migrate` 边界：Protocol、BlockName、StepName和MFA未变时，
  pending frame 无需 declaration、anchor、plan 或 migration API 自动继承。
- continuation、bind pattern、CaptureAbi 和 Env runtime values 不再参与 async identity；
  Capture/Env 问题归为执行环境或显式 Env migration，不把同一 StepName 重新
  分类为不同 async。
- `sf-migrate` 只处理named BlockName/StepName identity变化；无显式mapping时默认directional
  `incompatible_frame`，禁止
  相邻 index、AST/capture 相似度、bind name 或 runtime value fuzzy matching。
- historical/current selector 现在解析为 StepName；1-based `bind|then|let` source index
  仍用于人类定位，另外建立不计 let 的零基 async-only index。

## 2026-08-18

- 固定 transition 术语：`old` 始终是 historical/较旧版本，`new` 始终是 current/较新版本；
  upgrade 执行 old → new，downgrade 执行 new → old。
- downgrade 改为遵循 OTP dynamic callback advanced update：较新 module 在旧代码加载前执行
  `code_change({down, TargetVsn}, ...)`，完成业务 State downgrade 与 pending frame migration；
  成功后才允许加载旧 module。
- step mapping 增加 `directions` allowlist。up/down 可分别启用并配置独立 Env migrator，
  不再接受方向不明的 migrator，也不推断逆迁移函数。
- migration manifest 拆分 up/down plans；historical metadata 增加 downgrade 构造 target frame
  所需的 BlockName、StepName、MFA 与 CaptureAbi。downgrade 只写入 historical MFA，不提前执行。
- downgrade 调用方必须先生成 target historical State layout，并把 target callbacks offset
  传入 migration API。所有 current source frame、down plan 和 historical target metadata
  必须在 mutation 前完成原子预检；失败会阻止旧 module 加载。
- selector 增加统一计数 `bind|then|let` 的 `{index, N}`，以及唯一 name/content anchor
  后应用的正向 Offset；所有解析、范围检查和 let-target 拒绝均在编译期完成。
- declaration 和 step mapping 增加 `up_state` 与 `down_state`；`state => Name` 是两方向使用
  同一 capture name 的 shorthand。declaration-level 值是所有 step 的不可覆盖默认值。
- State-aware Env context 将 `old_state` 改名为 `source_state`：up 从 historical/old SourceEnv
  提取，down 从 current/new SourceEnv 提取；`current_state` 始终是传给
  `migrate_sf_callbacks/4` 的第四参数入口原值。该方向无 State 指令时 context 精确为
  `#{env => SourceEnv}`。
- 明确 State context 仅供 migrator 判断：runtime 不自动执行 State appup/appdown，也不把
  current_state 隐式写回 TargetEnv。
- terminal mismatch callback 收敛为只允许 abort，移除 replacement frame 路径。upgrade
  保留 implicit forced abort；downgrade 的不可映射 pending frame 默认报错，只有显式 policy
  abort 后才可继续。
- 明确功能尚未发布：directional manifest 直接成为唯一 format/protocol 1 schema，不升级
  版本、不兼容旧 flat-plan 开发草案，compiler/runtime 必须原子切换。
- 明确缺少 directional Env migrator 不是 compile error。真正运行中的 source frame/Env 只能
  由 runtime 观察；ABI 不同时生成 undefined migrator plan并进入 runtime mismatch。
- 固定 migration result 为 source/target block+async fields，移除含义随路径变化的 `block`；
  API error 增加 stage/reason/entry context，始终返回入口 TargetState与空 Results。
- current-anchor refresh 明确不依赖 historical steps，即使 `steps => []` 也只查询 current
  manifest。
- 整合项目改进约束：保持薄 facade与独立 `async_sf_migration`、复用已有 runtime helpers、
  条件生成 migration manifest并检查命名冲突、补充用户 README、结构化 preflight 诊断和
  feature-specific Dialyzer gate。
- 根据 master 已 revert sf-migrate 实现的仓库事实重写 tasks：保留 sf-do dependency gate，
  将其余工作分为可用 upgrade、selector、OTP downgrade、directional State context 四个可
  独立验收的纵向切片；最终命令只作为 initiative acceptance gate。
