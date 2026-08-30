# Tasks

当前master只有已完成的sf-do/sf-cc基础能力，尚无sf-migrate产品实现。任务只实现本次闭合后的
reply-time one-way bridge；旧文档中的eager State migration与downgrade plan不再是候选实现。

## Dependency gate

- [x] sf-do named identity固定为`{Protocol,Block,StepName}`，Vsn不进入identity。
- [x] sf-do current manifest提供owner/Vsn、generated MFA与required capture keys。
- [x] sf-cc使用完整`#sf_callback{frame,acc_ref,parent_ref,timer_ref}`和direct parent_ref routing。
- [x] runtime已经按每个frame owner做lazy current-manifest resolution，并允许target required Env subset。

## Task 1 — Exact historical declaration and raw bridge

**目标：** current FrameOwner能为一个exact old module Vsn保留完整old block，并把renamed old Step
原样转发到current Step，不扫描State或改写frame。

- [ ] 在`async_sf_transform`识别`sf_do_code_change/2`返回路径中的
  `sf_migrate(OldVsn, CompleteOldBody, #{from=>OldBlock,...})`；存在时自动export router。
- [ ] migration-enabled owner缺少显式`-vsn`时编译失败；OldVsn使用exact literal term，不做coercion。
- [ ] 复用sf-do frontend分析完整historical named block；不生成old workflow functions，不支持partial/
  unnamed block。
- [ ] 实现literal `#{to=>CurrentBlock,steps=>#{OldStep=>#{to=>CurrentStep}}}`，按StepName精确解析；
  拒绝index/Offset/content/generated atom和duplicate source declaration。
- [ ] 无converter时验证normalized Outcome shape相容且target required captures是old captures子集。
- [ ] 生成opaque plan和version-specific bridge；bridge原样转发Outcome/Env到current generated Step。
- [ ] 在`async_sf_runtime`中按exact StoredVsn/StoredBlock调用router；explicit mapped Step优先，plan未映射
  Step时回退现有automatic resolution。
- [ ] 验证成功路径不写callback/frame、不重发request、不重跑old qualifier，并继续原direct parent_ref。
- [ ] 添加exact Vsn、multiple case branches、Block/Step rename、raw bridge、unknown source、compile
  diagnostics和purged-old-code测试。

**独立验收：** A版本pending renamed Step在B加载并purge A后，通过B内声明的完整A block直接进入B
current Step；StateHolder和callback store没有migration写操作。

## Task 2 — Handwritten Outcome/Env converter and failure contract

**目标：** mapping可由用户同时转换Outcome与Env，expected incompatibility按sf-cc linked failure语义返回。

- [ ] 支持可选`convert => {M,F,2}`，固定返回
  `{ok,NewOutcome,NewEnv}|{error,Reason}`；不增加State/app context。
- [ ] 生成target Outcome shape validator和Env validator；NewEnv必须是atom-key map并包含target required
  captures，extra keys允许。
- [ ] same-identity explicit converter必须优先于automatic inheritance；匹配plan失败不得silent fallback。
- [ ] converter `{error,Reason}`、invalid return、incompatible Outcome/Env形成sanitized
  `{error,{sf_runtime,{incompatible_frame,Info}}}`并沿direct parent_ref传播。
- [ ] converter自身`error`/`exit`/`throw`保持sf-cc user exception口径，不被吞并。
- [ ] 添加Outcome-only、Env-only、同时转换、same-identity value conversion、missing converter、invalid
  output、converter error/exception和failure-handler tests。

**独立验收：** old callback内容保持不变；actual reply到达时bridge完成old `(Outcome,Env)`到current
`(Outcome,Env)`的用户定义转换，失败只影响当前linked callback。

## Task 3 — Downgrade and integration boundary

**目标：** 证明无需down migration：兼容frame在actual old target上直接恢复，不兼容frame在reply-time
failure，appdown阶段零sf-migrate副作用。

- [ ] 添加A→B→A测试：升级前A callback保留并在A恢复；B没有新callback时不发生任何fail。
- [ ] 添加B-created frame降到A测试：相同identity且B SavedEnv只增加captures时直接恢复并忽略extra；
  B删除A所需capture时structured failure。
- [ ] 验证down Outcome原样传入A Step，不调用reverse converter、不生成candidate、不读取target Vsn。
- [ ] 验证StateHolder `code_change({down,...})`不调用sf-migrate或`fail_sf_callbacks/4`，appdown不因pending
  incompatible callback被阻止。
- [ ] 添加multi-owner linked nodes、ordinary/stable mixed callbacks和partial release window tests。
- [ ] 搜索并确保无`migrate_sf_callbacks/4`、State offset、release_handler hook、version registry、
  `sf_migration_dependencies`或dependency graph residue。
- [ ] 同步README公开surface：declaration、router、converter、automatic compatibility、down boundary、
  failure与troubleshooting。
- [ ] 运行`rebar3 compile`、EUnit、Common Test、xref、dialyzer与
  `openspec validate sf-migrate --strict`。

**独立验收：** upgrade converter与双向automatic compatibility均通过真实callback测试；appdown没有
eager migration路径，所有公开文档与实现一致。

## Deferred

以下不属于本initiative，不建立placeholder实现：

- whole-project dependency/cycle warning Rebar plugin；
- automatic appup/relup ordering；
- reverse/down Outcome或Env converter；
- async runtime自身`#sf_callback{}` encoding migration。
