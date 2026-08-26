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
- [ ] public source 使用 `sf_do(Comprehension)` 与 `sf_do(BlockName, Comprehension)`；删除额外
  StepNames list/record 参数。
- [ ] 实现 inline literal atom StepName marker pairing/removal；duplicate、dangling、consecutive、
  pure-let/final-action marker 等非法 placement 在 Pass 1 产生 source-position error。
- [ ] unnamed non-final boundary 在 Pass 1 产生 warning；只有 warning 时继续 Pass 2 和普通执行。
- [ ] expected diagnostics 使用 Astranaut return channel 累计；Pass 1 有 error 时调用
  `astranaut_return:to_compiler/1` 并证明 Pass 2 未运行。
- [ ] 删除 Pass 2 的 source-form/name-marker/identity/capture 重复校验，只消费冻结 plan 与改写后 AST。

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

## 4. Dedicated sf_cc and callback execution

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

## 7. Upgrade failure boundary

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
- [ ] 增加 inline marker API/grammar、marker 不执行、unnamed warning/普通运行/migration failure、
  Action/index/binder changes 不影响 named identity，以及 BlockPath/StepName rename 改变 identity 的测试。
- [ ] 增加多错误 Pass 1 累计、`to_compiler/1` 输出和 Pass 2 未运行的回归测试。
- [ ] 增加 `case`/`receive`/`try` outer equality capture 与 fun/named-fun fresh shadowing 回归测试。
- [ ] `rebar3 compile`、EUnit、Common Test、xref、dialyzer 和 supported OTP matrix。

## 9. Optional follow-up: fun-valued captures

- [ ] 可选：为 Env 中 anonymous/local fun 提供 compile/runtime diagnostic metadata；不得把该检查
  变成当前 `sf_do` 的递归 value validation 或发布 gate。
- [ ] 可选：与 `sf-migrate` 联合验证 target-version fun regeneration；未启用该能力时，文档只要求
  应用避免 fun-valued capture 或改存 external MFA/data descriptor。
