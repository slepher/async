# Changelog

## 2026-08-31 — reply-time one-way migration contract（需求闭合，未实现）

- 以已实现sf-do/sf-cc为权威：named identity仍是`{Protocol,Block,StepName}`，module Vsn不参与
  identity；current target required captures是SavedEnv keys子集时自动恢复，extra Env允许。
- migration ownership从StateHolder移回FrameOwner。定义`sf_do`的module同时定义
  `sf_do_code_change/2`和`sf_migrate/3`；Holder不需要知道frame owner数量或目标版本。
- declaration收敛为
  `sf_migrate(OldModuleVsn, CompleteOldBody, #{from=>OldBlock,...})`。OldModuleVsn是exact literal
  module `-vsn`，不是app Vsn；多个historical Vsns使用多个普通Erlang case/clause分支，不建立
  version graph或multi-hop。
- Spec收敛为old/current StepName map与可选external `convert/2`。converter由用户手写并同时转换
  Outcome和Env；transform只派生old/current Outcome pattern shape、capture shape和Step MFA。
- migration从eager frame rewrite改为reply-time bridge。pending `#sf_callback`和frame不改写；原reply
  到达后generated old-interface bridge转换/复用`(Outcome,Env)`并直接调用current Step。
- explicit exact mapping优先于automatic inheritance，以支持same-identity value语义转换；没有matching
  mapping时仍回退sf-do automatic compatibility，不要求no-op declaration。
- 删除`async_m:migrate_sf_callbacks/4`、callbacks offset、State snapshot/context、migration results、
  eager enumeration/commit、on_mismatch、replacement/abort policy和第二migration manifest等旧草案。
- 删除bidirectional/down plans、target historical frame构造、reverse Env converter、target Vsn选择、
  candidate frame、release_handler私有状态读取和async version registry。
- app down阶段不扫描或fail callback。FrameOwner旧版真正加载后再按current manifest解析：相同identity
  且Env兼容则执行；否则callback触发时structured failure。down Outcome原样传递，不转换。
- 删除`-sf_migration_dependencies`、cycle prohibition与release ordering机制。未来whole-project Rebar
  plugin可提供warning，但不属于本change；错误顺序最多导致窗口内callback failure，不强制阻止release。
- ordinary do/callback、unnamed sf-do、callback store/record layout与linked parent_ref lifecycle保持不变。
- 本功能尚未发布；本条直接supersede 2026-08-17至2026-08-28的双向eager migration草案，不保留
  compatibility reader或deprecated API。
