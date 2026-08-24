# Tasks

现有 `{sf_program, EntryFrame}` / `{await|done}` 实现是前一版原型。以下任务按新的
same-Monad frame-bind contract 重新打开；旧测试可作为行为基线，但不能证明新结构已完成。

## 1. Compiler traversal and source model

- [x] `async_sf_transform` top-level traversal 改用 Astranaut `mapfold`，不再 generic-walk module
  forms。
- [ ] 拆分 collect/analyze/rewrite 两阶段，identity 始终基于 original source AST。
- [ ] 用 `{SourceFunction, LexicalBlockPath, OccurrenceIndex}` 对齐两遍 traversal，并以 original-node
  digest 检测 traversal/order mismatch。
- [ ] 第二遍只消费冻结 plan；generated functions 在 traversal 后追加且不再进入 source analysis。
- [ ] 收集 lexical nested block path、source function owner 和 anonymous occurrence。
- [ ] 支持 OTP 21 `sf_do/1`、`sf_do/2`、`sf_do/3` single-head comprehension forms。
- [ ] 实现 record/list complete StepNames 与 async-only assignment。

## 2. Same-Monad frame lowering

- [ ] 删除 `sf_program`、entry frame 和 `{await|done|abort}` instruction lowering。
- [ ] 把 qualifiers 直接编译为 `bind_frame`/`then_frame` chain，final expression 保持原 Monad。
- [ ] 为无绑定的非最终 Action 生成
  `monad_frame:then_frame(Action, IgnoreInputMFA, FrameContext, Monad)`；在
  `monad_frame` 提供直接委托 `bind_frame` 的 `default_then_frame`，并允许 instance override。
- [ ] atom/variable/二元 transformer Monad 都统一生成 `monad_frame:*` dispatcher call，不按 AST
  shape 直调 instance module，也不做 transform-time capability metadata 检查。
- [ ] 保留 lexical Monad stack；local `return/1`、`fail/1` 分别统一 rewrite 为
  `monad:return/2`、`monad_fail:fail/2`，remote call 与 block 外同名 call 不变。
- [ ] final `return(A)` 或任意 Monad action 直接返回；final generator/pure match 报 diagnostic。
- [ ] generated frame `(Input, Env)` 直接返回 remainder Monad。
- [ ] 明确 `monad_frame:bind_frame` 类型和 laws，确保 compatible facade 返回同类型 Monad。
- [ ] 明确 `monad_frame:then_frame` 类型、default delegation 和与 `classic_do` 无绑定 Action 的
  对照语义。
- [ ] ordinary do/bind 与 sf-do 可互相组合。

## 3. Pattern and lexical Env

- [ ] 用 scope-aware AST analysis 替换手写 `pattern_vars/free_vars` tuple recursion。
- [ ] capture 当前 continuation 实际 free bindings，不捕获 unused enclosing variables。
- [ ] generator pattern variables 与 `classic_do` 一样 shadow enclosing 同名 variables，不把它们
  restore 为 equality constraints。
- [ ] non-variable generator mismatch 返回 Monad `fail(monad_badmatch)`；pure let 保持 badmatch。
- [ ] Env key 直接使用 source variable atom，CaptureLayout 按 atom 排序；scope-aware analysis 只负责
  正确选择 capture set，并保证 generated internal variables hygienic；CaptureAbi 直接使用该 atom
  list，不做 capture SHA/digest。

## 4. Dedicated sf_cc and callback execution

- [ ] 定义 stable path 专有 `sf_cc` descriptor/invoker；不得复用 legacy CC fun、
  `callback_to_cc` 或 `run_cc` interface。
- [ ] stable runner 接收用户显式 external MFA/context `sf_cc`。
- [ ] `bind_frame` suspension 保存 generated `sf_frame` 与 downstream `sf_continuation` chain。
- [ ] 冻结 tagged-map `sf_frame`/`sf_cc`/`sf_next` encoding，callback 只保存一个以 `sf_cc` 终止的
  continuation chain；不兼容 prototype tuple 或双 continuation 字段。
- [ ] reply 恢复 frame、运行其返回 Monad，并保留下游 stable continuation。
- [ ] message 不推进 frame；timeout/DOWN 作为普通 action outcome 传入 frame。
- [ ] stable callback continuation slots 不使用 transform-generated bind fun 或 legacy CC adapter；
  不检查 Env/context 中任意 runtime value。
- [ ] 继续复用 callbacks field、request key、monitor、timer、accumulator 和 cleanup lifecycle。

## 5. Nested sf_do

- [ ] nested `sf_do` 作为普通 Monad Action 编译和运行。
- [ ] inner suspension 保存 outer downstream frame，inner completion 恢复 outer frame。
- [ ] nested named/anonymous block 使用 original lexical BlockPath，generated owner 不污染 identity。
- [ ] 覆盖两层以上 nesting、immediate inner、suspended inner、inner failure 和 timeout/DOWN。

## 6. Identity and manifest

- [ ] 实现 derived async-only index + canonical Action identity；pure let 不占 index；canonical term
  使用 protocol 1、去 annotation、binder alpha-normalization、map sorting 和 SHA-256。
- [ ] final action 不占 AsyncIndex/StepName，不生成 frame function 或 manifest entry。
- [ ] 实现 BlockPath + StepName named identity 和 distinct namespace；所有 generated function name
  统一使用 `'$sf_' ++ lowercase_hex(FullAsyncId)`。
- [ ] continuation、pattern、captures 和 Env 不参与 AsyncId/MFA。
- [ ] export `'$async_sf_manifest'/0`，记录 MFA、identity metadata 和排序后的 required capture schema，
  不记录 `sf_cc`、runtime Env、migration selector 或 historical data。
- [ ] automatic resume 使用 identity check + required-capture subset check，允许 extra Env keys。

## 7. Upgrade failure boundary

- [ ] 缺少 target required capture 时不重放 prefix action、不猜测值。
- [ ] `sf-migrate` Env migrator 必须在 resume 前生成 TargetEnv；`sf_do` 不定义 migration DSL、
  capture augmentation hook 或 retain-source 规则。
- [ ] 未完成 replacement 时精确返回 `{error, {appup, incompatible_frame}}` 或
  `{error, {appdown, incompatible_frame}}`。
- [ ] protocol/continuation/manifest/MFA 结构错误 cleanup 后恰好一次以
  `{error, {sf_runtime, Reason}}` 调用 terminal `sf_cc`。

## 8. Verification

- [ ] 更新 compiler fixtures，证明输出不包含 `sf_program`/`await`/`done` protocol。
- [ ] variable/non-variable pattern、同名 enclosing variable shadowing 与 `classic_do` 对照测试。
- [ ] local `return/1`、`fail/1`、统一 typeclass dispatch、nested Monad scope 和 remote call
  与 `do` 核心语义对照测试；不测试 `classic_do` 的 tuple-AST direct-call 特判。
- [ ] final return/arbitrary action 与 invalid final generator/match tests。
- [ ] `then_frame` lowering、default-to-`bind_frame` delegation、instance override 和 Reply discard tests。
- [ ] immediate、reply、message、timeout、DOWN 和 ordinary Monad failure tests。
- [ ] nested same-Monad composition tests。
- [ ] 检查 transformed bind/then continuation 和 callback continuation slots 不包含 transform
  生成的 anonymous remainder fun；不得递归拒绝 Env/Action/`sf_cc` context 中的任意 runtime term。
- [ ] 证明 stable completion 不调用 legacy CC adapter/runner。
- [ ] continuation-only change 保持 AsyncId/MFA；Env subset、缺失 capture、extra key、变量改名导致
  missing key，以及跨版本同名 key 默认直接复用分别验证；显式转换由 sf-migrate 测试负责。
- [ ] `rebar3 compile`、EUnit、Common Test、xref、dialyzer 和 supported OTP matrix。

## 9. Optional follow-up: fun-valued captures

- [ ] 可选：为 Env 中 anonymous/local fun 提供 compile/runtime diagnostic metadata；不得把该检查
  变成当前 `sf_do` 的递归 value validation 或发布 gate。
- [ ] 可选：与 `sf-migrate` 联合验证 target-version fun regeneration；未启用该能力时，文档只要求
  应用避免 fun-valued capture 或改存 external MFA/data descriptor。
