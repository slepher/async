# task-test-2 Contract

- Task: task-test-2
- Phase artifact: task-test-2.md
- Bundle generation: 2
- Goal: sf-cc
- Context: context-2.md
- Normative inputs: `openspec/changes/sf-cc/**`, `openspec/changes/sf-do/**`
- Evidence target: `0adf22a22f20c45fff192d52398f23f83bf4548c` plus accepted task-coding-2 merge-ready candidate
- Mode: hard
- Mode basis: R1–R19与sf-do linked semantics必须在同一public observation matrix中关联runner registration、action message/reply/timeout/DOWN、generated frame、shared-parent exactly-once、store backend与ordinary regression；拆分worker会丢失跨模块因果与统一acceptance gate。
- Execution strategy: single-sup
- Worker role: sup-coding-worker
- Lane: integrate
- Worktree: /home/slepher/project/erl_projects/async-sf-cc/.worktree/integrate
- Branch: codex/sf-cc-generation-2-integrate
- Base: task-coding-2 accepted merge-ready candidate merged atop 0adf22a22f20c45fff192d52398f23f83bf4548c
- Dependencies: task-coding-2 through lanes-2.md Covers
- Covers: task-coding-2
- Plan: plan-2.md
- Lane manifest: lanes-2.md

## Bounded goal

建立与旧测试不重复的document-driven public black-box acceptance：通过public `async_m` runners、sf-prefixed macros、inspection/failure facade、generated `sf_do` fixture modules及observable result/State/message/effect，证明R1–R19和全部sf-do linked contract；迁移只固定old representation的stale assertions，保留ordinary regression。测试不得以调用private dispatcher/helper或直接匹配internal records/maps作为唯一行为证明。

不修改production、OpenSpec、workflow/status、generation-1 artifacts；不实现fix。若测试揭示product defect，停止并按handoff报告exact public case，由dispatcher创建/路由fix task。

## Normative excerpts

测试oracle冻结为以下public semantics（`openspec/changes/sf-cc/specs/sf-cc/spec.md:5-465`、`openspec/changes/sf-do/specs/sf-do/spec.md:5-650`，由context-2.md E-1–E-3绑定）：

- 每次public runner invocation注册outer ContinuationRef；immediate path同次消费，suspended action以direct `parent_ref`链接；ordinary/wait source与正常result保持。
- action/node normal与failure路径都遵守preflight → take → direct cleanup → invoke/forward；absence返回unhandled，take后不rollback/retry。
- ordinary business Outcome/timeout/DOWN先进入current `cc`/frame；只有精确current availability/structural failure外移一层。message不消费action或node。
- inspection只公开request MRef、stable flag、direct parent、continuation kind与current frame Module/literal Vsn/Protocol/Block/StepName；matcher全量preflight后才mutation。
- active failure按selected Ref term order，stable status只能是`forwarded | handled | failed | unhandled`，ordinary返回`ordinary`且pending。
- `sf_do`保持Monad；generated `(Input,Env)` frame返回remainder Monad；bind/then、lexical return/fail、pattern、final action、nested composition与minimal Env语义保持。
- named identity精确`{1,Block,StepName}`，MFA精确`'$sf_<block>__<step>'/2`；manifest只公开current owner/literal Vsn/identity/MFA/required captures；unnamed migration前fail closed；two-pass error gate禁止Pass 2 artifacts。

## Context evidence

- E-14：existing `async_sf_cc_SUITE`覆盖old runner/cleanup/message/fail baseline，但summary、CcRef dedupe与terminal assertions已stale；可保留public behavior，必须替换old shape oracle。
- E-15：existing `async_sf_SUITE`覆盖source/lowering/pattern/capture与old lifecycle，但`sf_next` chain、SHA/hex MFA、block_path/async_id manifest assertions已stale。
- E-16：existing `async_t_SUITE`已有ordinary message/timeout/DOWN/parallel/accumulator/wait baseline；新case只补ordinary runner进入linked outer lifecycle及availability fallback，不重复整个matrix。
- E-13与macro suite：external source-form acceptance/rejection已有baseline；R3新增只验证AST-preserved fun/opaque Context与unified runtime lifecycle coupling。
- E-17/E-18：linked contract尚无code/test完成证据，且required symbols no-match；测试必须由current documents驱动而不是复刻old internal shape。

## Owned paths

- `test/async_sf_cc_SUITE.erl`
- `test/async_sf_cc_macro_SUITE.erl`
- `test/async_sf_SUITE.erl`
- `test/async_t_SUITE.erl`
- `test/async_sf_cc_linked_SUITE.erl`
- `test/async_sf_do_linked_SUITE.erl`
- `test/async_sf_cc_linked_fixture.erl`
- `test/async_sf_do_linked_fixture.erl`

`test/async_sf_transform_tests.erl`只作为unchanged formatter baseline执行，不编辑。所有`include/**`、`src/**`、OpenSpec、workflow/status与其他tests不属于本任务。

## Constraints and invariants

1. 新suite按public behavior断言，不把`#callback{}`、`#sf_callback{}`、private node map、reverse-index key或private helper return当作唯一oracle。需要证明linked关系时，使用public sanitized inspection、fail statuses、callback count、CleanState与late/duplicate observations。
2. fixture module可导出external callbacks、`sf_do` functions、literal/undefined Vsn与`'$async_sf_manifest'/0`的generated public behavior；不得导出测试专用private dispatcher入口。
3. existing source/macro/ordinary tests若仍准确则reuse；只迁移old-shape assertions或添加linked缺口。每个new test在coverage matrix中声明它不重复的E-14/E-15/E-16 gap。
4. concurrency tests必须有bounded receive timeout、唯一correlation token与after-cleanup，验证shared parent、late reply、duplicate fail、message-then-reply、parallel cancellation/orphan；不得依赖scheduler sleep作为唯一同步。
5. exception tests必须区分current invocation availability与callback内部异常：只允许前者转为sf_runtime failure；nested `badfun`/`undef`/business crash的class/reason/stack identity按public contract保留。
6. privacy tests使用sentinel secrets分别放入Env、State、timer/monitor metadata、accumulator、callback fun/Context，并递归确认summary不存在这些term；只断言documented keys，不通过缺少private helper字段推测实现。
7. matcher tests先收集稳定RequestMRef term order，验证exception/non-boolean时State bit-for-bit不变且callback未调用；commit tests验证同序statuses、ordinary pending与shared outer second arrival unhandled。
8. compile diagnostic tests以source form和compiler result为边界，覆盖Pass 1多error聚合/no Pass 2 artifacts、warning继续、duplicate primary/no derived noise、generated-name collision/length、unknown formatter ownership；不直接调用private analysis helpers。
9. manifest/identity tests只依赖generated exported function与`'$async_sf_manifest'/0`，验证move/insert/remainder变化稳定、rename变化、no hash/AsyncId/AST/index/parent/capture identity、sorted captures、literal Vsn；unnamed仅same-version resume且migration-mode public resolution fail closed。
10. ordinary regression必须继续覆盖existing message/timeout/DOWN/parallel/map/list/dict/accumulator/wait observable result；新增最多一组cross-lifecycle matrix，不复制E-16所有cases。
11. product failure、crash、resource leak、wrong public result/status/privacy leak或unsupported backend不是可由test task接受的transition；停止并提交exact reproduction，不改production。

## R1–R19 public coverage matrix

| Requirement | Document-driven observable case | Existing coverage treatment |
|---|---|---|
| R1 dedicated sf runners | ordinary anonymous remainder与stable frame remainder都经同一sf-prefixed final callback一次 | 扩展E-14，不重复mode smoke test |
| R2 every runner outer node | `wait/1`、`wait_t/2`、ordinary runner family与三种sf runner各测immediate无pending、suspended direct parent/one completion | E-16 ordinary result复用，只新增outer lifecycle observation |
| R3 macro source gate | 保留external/local/anonymous/variable/arity diagnostics，并新增external fun term/opaque Context贯穿suspension | 复用macro suite，新增未覆盖coupling |
| R4 descriptor fun/Context | callback收到term-identical opaque Context，purge/inspection不把Context当Env且不泄露 | 替换old descriptor-shape introspection |
| R5 mode returns | async `/2` effect读CleanState；direct `/2`忽略return；direct `/3`接受compatible State并拒绝async_r_t/invalid after-take | 扩展E-14 exact contracts |
| R6 action current/direct parent | ordinary current `cc`与stable current frame在nested completion中各只走direct parent | 用public effects/summary证明，不直接匹配record |
| R7 AccRef independence | parallel/local accumulator重绑定后continuation parent不变且accumulator result正确 | 复用E-16 baseline，新增cross-scope assertion |
| R8 explicit context | classic do under sf runner、stable context中的ordinary bind、frame-to-ordinary bridge均正确恢复 | 新linked gap；不检查process dictionary implementation |
| R9 one Ref type | sanitized parent refs可跨stable/ordinary/outer chain，internal Ref作为external correlation返回unhandled | 替换old kind list assertion |
| R10 dispatch order | `/3` CleanState看不到current node/action/direct timer-monitor-accumulator resources | 迁移E-14 cleanup case到node-local semantics |
| R11 exactly once | callback reply-then-crash、invalid return、late reply、duplicate fail、shared parent都不二次invoke | 扩展E-14，新增shared-parent/after-take cases |
| R12 message retention | message先执行continuation但action/node仍pending，后续reply完成一次 | 复用E-14 baseline并增加stable linked observation |
| R13 one-level availability | current ordinary term-exact badfun与frame structural failure只跳一层；business error仍进入current | 新linked gap |
| R14 nested linked frames | nested modules/frames逐node恢复，current-only summary不含outer payload/root duplication | 替换`[sf_next,sf_next,sf_cc_ref]` assertion |
| R15 reader/classifier | map/list/dict下stable/ordinary action可reply，intermediate/outer Ref不可external reply，pending count只含actions | 扩展E-16 backend baseline |
| R16 final availability exact | nofile/missing export规范化；other load error与callback-internal badfun/undef原样传播 | 扩展E-14 exception baseline |
| R17 safe inspection | current frame fields与literal Vsn正确，ordinary frame undefined，privacy sentinels全不可见；显式chain若public则bounded/sanitized | 替换old summary keys |
| R18 matcher atomicity | term-order snapshot；late matcher exception/non-boolean后State bit-for-bit不变且零callback | 复用并强化E-14 preflight case |
| R19 active failure | selected-order status、ordinary pending、shared outer一次、duplicate unhandled、exception停止且已消费不回滚 | 替换CcRef dedupe oracle |

## sf-do linked-contract coverage

1. 编译并运行bind/then/two-boundary/final arbitrary Monad action，证明original Monad type与generated frame直接返回remainder Monad；final action不生成function/manifest entry。
2. nested lexical Monad的local `return/1`/`fail/1`、remote/outside calls、generator mismatch与pure-let mismatch分别保持documented semantics；复用已有source cases，只增加linked resume path。
3. classic do、nested `sf_do`、ordinary downstream、timeout/DOWN/message在direct parent链上按Monad composition恢复；不同owner module逐nodelazy resolution且未执行node保持不变。
4. capture fixtures覆盖unused exclusion、Input/generator fresh bindings、case/receive/try equality、anonymous/named fun shadow、sorted atom CaptureAbi、extra Env允许与required missing fail-before-invoke。
5. named fixture验证`{1,Block,StepName}`、`'$sf_<block>__<step>'/2`、move/insert/remainder稳定、rename变化、module-wide Block/Step/function collision、atom length diagnostic及identity不含禁止字段。
6. manifest fixture验证single public shape、owner、literal tuple Vsn/`undefined`、identity/MFA/required captures；不含AsyncId/hash/digest/full AST/runtime Env/final descriptor/historical mapping。
7. unnamed fixture只验证ordinary/same-version resume；在migration resolution mode下调用前fail closed并向direct parent提交documented structural outcome，不定义sf-migrate converter细节。
8. Pass 1 fixtures验证multiple errors aggregated且Pass 2/generated functions/manifest未产生，warning-only继续；Pass 2 ordering只消费frozen occurrence/digest/plan，generated AST不被source reanalysis；diagnostic reason/presentation owner保持。
9. shared callback lifecycle、all runners与additive compatibility由R1–R19 matrix共同闭合；ordinary anonymous callback不被宣传为stable migration frame。

## Ordered work

1. 建立coverage ledger，将R1–R19与sf-do requirements逐项映射到existing reused case、stale case replacement或new public scenario；先标出E-14–E-16避免重复的baseline。
2. 更新existing sf-cc/sf-do suites：保留仍规范的runner/source/ordinary assertions，删除/替换`sf_cc_ref`、`sf_next`、`sf_cc_mref`、old frames summary、block_path/async_id与SHA identity oracle；不得简单把old private shape换成new private shape。
3. 新建linked sf-cc fixture/suite，按R1–R19 matrix覆盖all-runner immediate/suspended、modes、direct cleanup、ordinary/stable bridge、one-level availability、classifier backends、exactly-once/shared parent、inspection/privacy/matcher/fail statuses。
4. 新建linked sf-do fixture/suite，覆盖Monad/lowering/nesting/pattern/capture、direct identity/MFA/manifest、literal Vsn/owner resolution、unnamed fail-closed与two-pass diagnostics；所有runtime outcome经public runner观察。
5. 在existing ordinary suite只增加一个cross-lifecycle regression group，证明wait/ordinary API在linked implementation下的message/timeout/DOWN/parallel/accumulator正常行为；其余复用existing coverage。
6. 先运行focused suites并消除test asset/harness问题；若失败指向production contract，停止并记录exact command/case/class/reason/public observation，不修改`src/**`。
7. 运行完整blocking matrix及supported OTP matrix；保存exit、suite counts、R1–R19/sf-do ledger、nondup说明、flakiness controls与`git diff --check`。

## Validation evidence

handoff必须提供：R1–R19逐项test case/function和result；sf-do requirement group到fixture/function/result；existing test reuse/stale replacement/nondup表；各public runner immediate/suspended结果；callback count与late/duplicate/shared-parent trace；CleanState direct-resource absence；privacy sentinel search；exception class/reason/stack observation；map/list/dict result；manifest sample；compile diagnostic sample；所有blocking command exit与supported OTP matrix结果。

## Blocking self-validation

- Commands: `rebar3 compile`; `rebar3 eunit`; `rebar3 ct --suite=test/async_sf_cc_macro_SUITE.erl`; `rebar3 ct --suite=test/async_sf_cc_SUITE.erl`; `rebar3 ct --suite=test/async_sf_cc_linked_SUITE.erl`; `rebar3 ct --suite=test/async_sf_SUITE.erl`; `rebar3 ct --suite=test/async_sf_do_linked_SUITE.erl`; `rebar3 ct --suite=test/async_t_SUITE.erl`; `rebar3 xref`; `rebar3 dialyzer`; `rebar3 docker_ci`; `git diff --check`

## Deferred downstream validation

- Commands: none
- Owner: none
- Expected transitional failures: none
- Closure gate: none

## Observable completion

- R1–R19与sf-do linked-contract ledger无空项，每项至少有一个public observable case且注明existing reuse或new gap；没有用private shape断言替代行为证明。
- coding contract列出的old assertion transition全部关闭；ordinary source/result/message/timeout/DOWN/parallel/backend/accumulator regressions继续通过。
- focused suites、full EUnit/Common Test、xref、dialyzer、supported OTP matrix与format checks全部exit 0；无accepted transitional failure。
- tests只修改owned paths；没有production fix、status/OpenSpec/workflow修改、stage/commit/merge。

## Stop conditions

- task-coding-2 candidate未合并到fixed integrate lane，或revision/base与manifest不符。
- public behavior与current normative inputs存在material ambiguity，或测试需要读取/定义`sf-migrate` contract。
- 任一case暴露production defect、privacy leak、resource leak、wrong result/status、unexpected exception、flaky race或unsupported backend；记录evidence并停止，不在test task修production。
- 完成需要修改owned paths之外的test harness/config或任何production path。
- supported OTP matrix command缺失/环境不可用，或blocking command出现无法在owned test assets中修复的baseline failure。
- worktree含与owned test path重叠且无法安全保留的unrelated change。

## Handoff

向dispatcher返回integrate candidate identity、R1–R19与sf-do coverage ledger、existing-vs-new nondup表、全部blocking outputs和零deferred声明。product failure时返回exact reproduction与affected `task-coding-2`，不自行创建fix、修改status、stage/commit、accept generation或record-draft。
