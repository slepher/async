# Tasks

现有 `{sf_program, EntryFrame}` / `{await|done}` 实现是前一版原型。以下任务按新的
same-Monad frame-bind contract 重新打开；旧测试可作为行为基线，但不能证明新结构已完成。

## 1. Compiler traversal and source model

- [x] `async_sf_transform` top-level traversal 改用 Astranaut `mapfold`，不再 generic-walk module
  forms。
- [x] 拆分 collect/analyze/rewrite 两阶段，identity 始终基于 original source AST。
- [x] 用 `{SourceFunction, LexicalBlockPath, OccurrenceIndex}` 对齐两遍 traversal，并以 original-node
  digest 检测 traversal/order mismatch。
- [x] 第二遍只消费冻结 plan；generated functions 在 traversal 后追加且不再进入 source analysis。
- [x] 收集 lexical nested block path、source function owner 和 anonymous occurrence。
- [x] public source 使用 `sf_do(Comprehension)` 与 `sf_do(BlockName, Comprehension)`；删除额外
  StepNames list/record 参数。
- [x] 实现 inline literal atom StepName marker pairing/removal；duplicate、dangling、consecutive、
  pure-let/final-action marker 等非法 placement 在 Pass 1 产生 source-position error。
- [x] unnamed non-final boundary 在 Pass 1 产生 warning；只有 warning 时继续 Pass 2 和普通执行。
- [x] expected diagnostics 使用 Astranaut return channel 累计；Pass 1 有 error 时调用
  `astranaut_return:to_compiler/1` 并证明 Pass 2 未运行。
- [x] 删除 Pass 2 的 source-form/name-marker/identity/capture 重复校验，只消费冻结 plan 与改写后 AST。
- [x] 将 transform-owned reason 收敛为严格 `format_error/1` 精确 clauses，删除领域 catch-all；由
  Astranaut adapter 独占 compiler fallback，并让测试直接调用领域 formatter 暴露 `function_clause`。
- [x] 去除 duplicate StepName/BlockPath 与 trailing marker 的派生 diagnostics，同时保持不同 occurrence
  的独立错误累计；ordering mismatch 只报告有界摘要，不打印完整 rewrite state。

## 2. Same-Monad frame lowering

- [x] 删除 `sf_program`、entry frame 和 `{await|done|abort}` instruction lowering。
- [x] 把 qualifiers 直接编译为 `bind_frame`/`then_frame` chain，final expression 保持原 Monad。
- [x] 为无绑定的非最终 Action 生成
  `monad_frame:then_frame(Action, IgnoreInputMFA, FrameContext, Monad)`；在
  `monad_frame` 提供直接委托 `bind_frame` 的 `default_then_frame`，并允许 instance override。
- [x] atom/variable/二元 transformer Monad 都统一生成 `monad_frame:*` dispatcher call，不按 AST
  shape 直调 instance module，也不做 transform-time capability metadata 检查。
- [x] 保留 lexical Monad stack；local `return/1`、`fail/1` 分别统一 rewrite 为
  `monad:return/2`、`monad_fail:fail/2`，remote call 与 block 外同名 call 不变。
- [x] final `return(A)` 或任意 Monad action 直接返回；final generator/pure match 报 diagnostic。
- [x] generated frame `(Input, Env)` 直接返回 remainder Monad。
- [x] 明确 `monad_frame:bind_frame` 类型和 laws，确保 compatible facade 返回同类型 Monad。
- [x] 明确 `monad_frame:then_frame` 类型、default delegation 和与 `classic_do` 无绑定 Action 的
  对照语义。
- [x] ordinary do/bind 与 sf-do 可互相组合。

## 3. Pattern and lexical Env

- [ ] 修正 scope-aware capture：`case`/`receive`/`try` 已绑定 pattern variable 是 equality reference，
  `fun`/named-fun clause parameter 是 fresh binding；复用统一 lexical binding classifier。
- [x] capture 当前 continuation 实际 free bindings，不捕获 unused enclosing variables。
- [x] generator pattern variables 与 `classic_do` 一样 shadow enclosing 同名 variables，不把它们
  restore 为 equality constraints。
- [x] non-variable generator mismatch 返回 Monad `fail(monad_badmatch)`；pure let 保持 badmatch。
- [x] Env key 直接使用 source variable atom，CaptureLayout 按 atom 排序；scope-aware analysis 只负责
  正确选择 capture set，并保证 generated internal variables hygienic；CaptureAbi 直接使用该 atom
  list，不做 capture SHA/digest。

## 4. Implemented legacy sf_cc baseline (superseded)

本节记录 2026-08-26 前已经实现的 full-descriptor terminal，保留为迁移证据；它不是新的目标合同。

- [x] 定义 stable path 专有 `sf_cc` descriptor/invoker；不得复用 legacy CC fun、
  `callback_to_cc` 或 `run_cc` interface。
- [x] stable runner 接收用户显式 external MFA/context `sf_cc`。
- [x] `bind_frame` suspension 保存 generated `sf_frame` 与 downstream `sf_continuation` chain。
- [x] 冻结 tagged-map `sf_frame`/`sf_cc`/`sf_next` encoding，callback 只保存一个以 `sf_cc` 终止的
  continuation chain；不兼容 prototype tuple 或双 continuation 字段。
- [x] reply 恢复 frame、运行其返回 Monad，并保留下游 stable continuation。
- [x] message 不推进 frame；timeout/DOWN 作为普通 action outcome 传入 frame。
- [x] stable callback continuation slots 不使用 transform-generated bind fun 或 legacy CC adapter；
  不检查 Env/context 中任意 runtime value。
- [x] 继续复用 callbacks field、request key、monitor、timer、accumulator 和 cleanup lifecycle。

## 5. Nested sf_do

- [x] nested `sf_do` 作为普通 Monad Action 编译和运行。
- [x] inner suspension 保存 outer downstream frame，inner completion 恢复 outer frame。
- [x] nested explicit/anonymous-path block 使用 original lexical BlockPath，generated owner 不污染 identity。
- [x] 覆盖两层以上 nesting、immediate inner、suspended inner、inner failure 和 timeout/DOWN。

## 6. Identity and manifest

- [ ] 删除 derived identity、Action canonicalizer、annotation/alpha/map normalization 和
  AsyncForm digest；source-order index 不参与 identity。
- [x] final action 不消费 StepName，不生成 frame function 或 manifest entry。
- [ ] named identity 仅使用 `{sf_named_async, Protocol, BlockPath, StepName}`；unnamed boundary 使用
  non-migratable local frame locator/MFA，并在 manifest 记录 `step_name => undefined`。
- [x] continuation、pattern、captures 和 Env 不参与 AsyncId/MFA。
- [ ] export `'$async_sf_manifest'/0`，记录 `StepName | undefined`、MFA 和排序后的 required capture schema；
  删除 identity mode、AsyncIndex 和 AsyncForm digest。
- [x] automatic resume 使用 identity check + required-capture subset check，允许 extra Env keys。

## 7. Implemented legacy upgrade failure boundary (superseded)

- [x] 缺少 target required capture 时不重放 prefix action、不猜测值。
- [x] `sf-migrate` Env migrator 必须在 resume 前生成 TargetEnv；`sf_do` 不定义 migration DSL、
  capture augmentation hook 或 retain-source 规则。
- [x] 未完成 replacement 时精确返回 `{error, {appup, incompatible_frame}}` 或
  `{error, {appdown, incompatible_frame}}`。
- [x] protocol/continuation/manifest/MFA 结构错误 cleanup 后恰好一次以
  `{error, {sf_runtime, Reason}}` 调用 terminal `sf_cc`。

## 8. Verification

- [x] 更新 compiler fixtures，证明输出不包含 `sf_program`/`await`/`done` protocol。
- [x] variable/non-variable pattern、同名 enclosing variable shadowing 与 `classic_do` 对照测试。
- [x] local `return/1`、`fail/1`、统一 typeclass dispatch、nested Monad scope 和 remote call
  与 `do` 核心语义对照测试；不测试 `classic_do` 的 tuple-AST direct-call 特判。
- [x] final return/arbitrary action 与 invalid final generator/match tests。
- [x] `then_frame` lowering、default-to-`bind_frame` delegation、instance override 和 Reply discard tests。
- [x] immediate、reply、message、timeout、DOWN 和 ordinary Monad failure tests。
- [x] nested same-Monad composition tests。
- [x] 检查 transformed bind/then continuation 和 callback continuation slots 不包含 transform
  生成的 anonymous remainder fun；不得递归拒绝 Env/Action/`sf_cc` context 中的任意 runtime term。
- [x] 证明 stable completion 不调用 legacy CC adapter/runner。
- [x] continuation-only change 保持 AsyncId/MFA；Env subset、缺失 capture、extra key、变量改名导致
  missing key，以及跨版本同名 key 默认直接复用分别验证；显式转换由 sf-migrate 测试负责。
- [x] 增加 inline marker API/grammar、marker 不执行、unnamed warning/普通运行/migration failure、
  Action/index/binder changes 不影响 named identity，以及 BlockPath/StepName rename 改变 identity 的测试。
- [x] 增加多错误 Pass 1 累计、`to_compiler/1` 输出和 Pass 2 未运行的回归测试。
- [x] CT 增加 non-literal block、non-comprehension、invalid Monad、duplicate BlockPath 和直接
  generated-function conflict 的真实 compiler 回归；EUnit 增加 defensive AST、严格 formatter 精确文本
  和 Astranaut fallback 单元测试。
- [ ] 增加 `case`/`receive`/`try` outer equality capture 与 fun/named-fun fresh shadowing 回归测试。
- [ ] `rebar3 compile`、EUnit、Common Test、xref、dialyzer 和 supported OTP matrix。

## 9. Optional follow-up: fun-valued captures

- [ ] 可选：为 Env 中 anonymous/local fun 提供 compile/runtime diagnostic metadata；不得把该检查
  变成当前 `sf_do` 的递归 value validation 或发布 gate。
- [ ] 可选：与 `sf-migrate` 联合验证 target-version fun regeneration；未启用该能力时，文档只要求
  应用避免 fun-valued capture 或改存 external MFA/data descriptor。

## 10. sf-cc integration and self-describing resolution (not implemented)

- [ ] 删除 sf-do 对完整 sf_cc descriptor、`invoke_sf_cc/2` 和 public `run_sf_frame/4` final API 的
  ownership；所有 public runners 委托统一 CcRef core，显式 stable execution 衔接
  `run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5`。
- [ ] 将 `wait/1`、`wait_t/2` 通过 library-owned external completion 无感迁移到 CcRef；保持阻塞、
  timeout 和正常 Outcome contract。
- [ ] 将旧 `run_cc`/`run_with_cc`/`exec_cc`/`run` 和 anonymous Callback forms 作为 compatibility
  facades 接入 CcRef；保持原 argument/callback/return normalization，但不宣称 anonymous final fun
  具有 explicit external callback guarantee。
- [ ] 把 continuation terminal 改为
  `#{kind => sf_cc_ref, protocol => 1, mref => CcRef}`，不得保留 full-descriptor 双 owner。
- [ ] stable suspension 在 `#callback.cc` 保持 frame chain，并在 `#callback.sf_cc_mref` 保存相同 CcRef；
  resume 后产生的 action callback 必须继承 CcRef，AccRef 重绑定不得覆盖它；共享 CcRef 不得实现为
  多 Promise operation group 或 per-entry final completion。
- [ ] 所有 callback readers 显式接受旧 `{callback, Cc, AccRef}` 和新四元 tuple；旧 tuple 解码为
  unprotected，不从 Cc/frame 推断 CcRef。
- [ ] 对已 pending old tuple 增加 unavailable-code classifier；确认旧 anonymous continuation 因 purge
  不可调用时清理该 request，禁止 crash/retry/final CC，并让 `async_m:handle_info/3` 返回
  `{legacy_outcome, OriginalActionOutcome, CleanState}`。不得吞掉已经进入 callback 后的普通异常。
- [ ] frame structural failure 直接使用 decoded callback.sf_cc_mref 调用 sf-cc completion；不得递归
  遍历损坏 chain，也不得自行定义 cleanup、retry 或 callback return semantics。
- [ ] 为 `sf_frame` 与 `'$async_sf_manifest'/0` 增加 owner Module 和 explicit module Vsn literal term/
  `undefined`；验证 frame Module 与 MFA owner 一致，且 Vsn 不进入 AsyncId。
- [ ] 在每次 generated MFA invocation 前增加 owner Module/Vsn resolution hook；same-version fast path
  仍验证 identity、MFA 和 required capture subset。
- [ ] 支持 holder process 未执行 `code_change/3` 时的 reply-time lazy resolution；nested multi-module
  chain 逐节点 resolve，非当前 owner nodes 保持不变。
- [ ] upgrade resolution 接受 actual stored frame 作为 source endpoint，不要求 historical source block；
  historical metadata 只作为 sf-migrate diagnostics/downgrade input。
- [ ] 允许 resolver 使用 sf-migrate 产生的 TargetEnv/TargetInput，但不得在 sf-do 中冻结 converter
  syntax、return shape、version-chain composition 或 strict downgrade policy。
- [ ] resolution/migration failure 不执行 target MFA，并通过 CcRef terminal-complete；migration Outcome
  由 sf-migrate 定义，structural Outcome 保持 `{error, {sf_runtime, Reason}}`。
- [ ] 增加 sf_cc_ref encoding、CcRef consistency、old tuple、literal Vsn、external owner upgrade、
  universal wait/ordinary runner protection、purged old fun legacy outcome、multi-module nesting、
  no-historical-block upgrade 和 completion handoff integration tests。
