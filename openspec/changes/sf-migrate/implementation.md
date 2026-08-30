# sf-migrate Implementation Contract

本实现直接扩展已完成的 sf-do/sf-cc。不得复制 frame record、current manifest、callback decoder、
ContinuationRef router或 lifecycle cleanup。

## 0. Responsibility Boundary

`async_sf_transform` owns：

- `sf_do_code_change/2` 中 `sf_migrate/3` special form discovery；
- complete historical `sf_do/2` analysis；
- exact source Vsn/block与Step mapping validation；
- Outcome/Env shape derivation；
- opaque plan与bridge generation。

`async_sf_runtime` owns：

- per-frame current owner/manifest resolution；
- exact plan request/validation；
- explicit bridge优先和automatic compatibility fallback；
- expected incompatibility的structured failure。

`async_sf_cc` 继续独占：

- `#sf_callback{}` decode/take；
- direct `parent_ref` routing；
- timer、monitor、accumulator与node cleanup；
- failure handler和user exception语义。

StateHolder、`async_m:fail_sf_callbacks/4` 与任何 State offset API 不参与 sf-migrate。

## 1. Accepted Source Surface

migration-enabled FrameOwner MUST 有一个显式 module `-vsn` attribute，并定义
`sf_do_code_change/2`。transform MUST 自动 export该 function；用户重复 export允许，重复 function
definition仍按 Erlang现有规则报错。

`sf_do_code_change/2` 是普通 Erlang function。transform只特殊处理其返回路径内的：

```erlang
sf_migrate(OldModuleVsn, OldComprehension, Spec)
```

要求：

- `OldModuleVsn` 是 literal Erlang term；
- `OldComprehension` 是完整合法 sf-do comprehension；
- `Spec` 是 literal map；
- `Spec` keys精确为 `from`、`to` 与 `steps`；
- `from` 是 historical named block atom；
- `to` 是 current named block atom；
- `steps` 是 `#{OldStepName => TargetSpec}`；
- `TargetSpec` keys精确为 `to` 和可选 `convert`；
- `TargetSpec.to` 是 current StepName atom；
- `convert` 是可 external-call 的 `{Module, Function, 2}` literal MFA。

第一版不接受 list/tuple shorthand、version list、directions、state、transition、on_mismatch、index、
Offset、content selector、anonymous block或generated function atom。

`sf_migrate/3` 在其他 function、非返回expression或嵌套为普通参数时 MUST 报
`invalid_sf_migrate_placement`，避免运行时误调用不存在的普通 function。

每个 `{OldModuleVsn, Spec.from}` 只能声明一次；同一 declaration 中 old Step map key天然唯一，
多个 old Step不得映射到同一个 target限制不成立，因为多个历史边界可以合法汇合到同一 current Step。

最低诊断集合：

- `sf_migrate_requires_explicit_vsn`；
- `invalid_sf_migrate_syntax`；
- `invalid_sf_migrate_placement`；
- `duplicate_sf_migration_source`；
- `sf_historical_block_not_named`；
- `sf_current_block_not_found`；
- `sf_step_not_found`；
- `invalid_sf_migration_converter`；
- `sf_migration_converter_required`；
- `sf_migration_generated_collision`。

所有 diagnostic payload必须是Vsn/Block/Step/MFA等有界metadata，不得包含完整 historical AST。

## 2. Shared Historical Analysis

historical `sf_do` 必须复用 current sf-do 的 qualifier、inline StepName、pattern、capture和tail
analysis frontend。它只产生 frozen boundary metadata，不生成old workflow functions、exports、requests
或executable qualifiers。

每个 historical named boundary至少冻结：

```erlang
#{
    identity => {1, OldBlock, OldStep},
    outcome_shape => OldOutcomeShape,
    required_captures => OldCaptureNames,
    historical_mfa => {Owner, OldGeneratedFunction, 2}
}
```

`historical_mfa` 仅用于验证actual stored frame来自声明的old boundary；新 module不得生成或调用该
old continuation body。

current target metadata必须从本次current sf-do frozen plans/current manifest source生成，不得从用户
Spec复制。target不存在、unnamed或generated MFA collision均在编译期失败。

## 3. Raw Forwarding Versus Converter

未配置 converter时，compiler MUST 证明：

```text
normalized OldOutcomeShape =:= normalized NewOutcomeShape
and TargetRequiredCaptures is_subset_of OldCaptureNames
```

Outcome normalization只归一化pattern annotation和variable binder名称，不做term type inference、AST
相似度或业务等价推断。无法证明时必须要求converter，不得把问题推迟到old code purge后。

配置 converter时，无论identity/capture shape是否相同都必须生成并使用converter bridge；这允许
same-identity value语义转换。compiler只验证MFA形式，不执行converter。

converter contract：

```erlang
-spec convert(term(), map()) ->
    {ok, term(), map()} | {error, term()}.
```

runtime必须验证NewEnv是atom-key map、包含current target全部required captures，并用generated target
Outcome matcher验证NewOutcome。Source/Target Env extra keys允许。

converter返回`{error, Reason}`是designed migration failure；其他return是
`invalid_converter_result`。converter自身`error`/`exit`/`throw`按sf-cc既有user exception规则传播，
不得伪装为incompatible frame。

## 4. Generated Plan and Bridge

每个 `sf_migrate/3` expression编译为opaque plan value。用户只能原样return，不能依赖其term shape。
runtime内部至少需要等价metadata：

```erlang
#{
    kind => sf_migration_plan,
    format => 1,
    owner => Owner,
    source_vsn => OldModuleVsn,
    source_block => OldBlock,
    target_vsn => CurrentModuleVsn,
    target_block => CurrentBlock,
    steps => #{
        OldStep => #{
            source_identity => {1, OldBlock, OldStep},
            source_required_captures => OldCaptureNames,
            target_identity => {1, CurrentBlock, CurrentStep},
            bridge_mfa => {Owner, BridgeFunction, 2}
        }
    }
}.
```

该shape是compiler/runtime内部contract，不是公开API；完整OldBlock AST、Outcome/Env runtime values和
converter fun不得放入plan。converter只保存external MFA。

BridgeFunction必须version-specific或由唯一PlanId命名，避免多个source Vsn共享old identity时碰撞。
用户已有同名function时compile-time collision，不得覆盖。

bridge `/2`：

1. 验证actual OldOutcome/OldEnv符合source boundary；
2. converter存在则调用，否则原样使用；
3. 验证NewOutcome/NewEnv符合current target；
4. external-call current generated Step MFA；
5. 返回current Step产生的`{async_t,_}`，或内部tagged designed failure供runtime转发。

bridge不得返回replacement frame、更新callback store、dispatch parent或执行old Step body。

## 5. Runtime Resolution Algorithm

在`async_sf_runtime:continue/5`的现有per-owner resolution boundary中实现，输入仍是单个stored frame、
direct ParentRef、Outcome和MR。不得新增whole-State facade。

一次resolution使用同一个current manifest snapshot：

1. 按sf-do现有规则验证stored frame并加载`Frame.module`；
2. 调用current `'$async_sf_manifest'/0`并验证owner/protocol/format；
3. `StoredVsn =:= CurrentVsn`时只走现有same-version path，不调用router；
4. Vsn不同时，若current Owner export `sf_do_code_change/2`，调用
   `Owner:sf_do_code_change(StoredVsn, StoredBlock)`；
5. 返回opaque plan时，验证plan owner/source Vsn/source Block/target Vsn均与本次frame和manifest一致；
6. plan显式包含StoredStep时，验证stored identity、source required Env并调用bridge；
7. router缺失、返回`unhandled`或plan没有StoredStep时，执行sf-do automatic named resolution；
8. automatic path要求相同`{Protocol,Block,StepName}`、current MFA存在且
   `CurrentRequiredCaptures ⊆ maps:keys(SavedEnv)`；
9. 两条路径均不可用时，通过现有`structural_failure/3`交给direct ParentRef。

显式mapping优先于automatic path。plan已匹配StoredStep但plan/bridge验证失败时不得静默退回automatic
path，因为用户已明确要求该source step转换。

router返回其他term属于invalid plan。router自身exception按普通external user code exception传播。

## 6. No Stored-Frame Mutation

sf-migrate不得调用callbacks getter/setter，不得枚举ActionMRef或ContinuationRef，也不得写回
`#sf_callback.frame`。bridge只在node已被sf-cc正常take并收到Outcome后运行。

因此以下数据继续完全由sf-cc管理：

- callback map key；
- `parent_ref`；
- `acc_ref`；
- `timer_ref`、monitor和request；
- outer/internal continuation nodes；
- ordinary callback entries。

每个outer stable node将来轮到执行时，再根据自己的FrameOwner/Vsn独立resolve。

## 7. Downgrade

本实现不增加appdown入口。StateHolder的`code_change({down,...})`不得因sf-migrate扫描或fail
callbacks。

FrameOwner降级后仍执行步骤5的current-manifest resolution：

- identity相同且target required Env是saved Env keys子集：直接恢复；
- extra SavedEnv fields忽略；
- Outcome原样传入target Step；
- 其他情况structured incompatible failure。

不调用较新module converter，不读取old code slot，不构造down candidate，不反转source→current bridge。
core runtime不比较Vsn大小，因此也不把任意term解释为版本顺序。

## 8. Failure Shape

expected failure reason必须归一化为：

```erlang
{incompatible_frame, #{
    module := module(),
    stored_vsn := term(),
    current_vsn := term(),
    identity := term(),
    reason := no_compatible_plan
            | invalid_historical_frame
            | converter_failed
            | invalid_converter_result
            | incompatible_outcome
            | incompatible_env
}}
```

然后复用`async_sf_runtime:structural_failure/3`形成
`{error,{sf_runtime,Reason}}`并交给direct parent。Info不得包含Env、Outcome、State、callback map或fun。

missing current manifest/MFA、unsupported protocol和corrupt frame继续沿用sf-do既有structural failure，
不重分类为migration declaration error。

## 9. Release Ordering

不生成`-sf_migration_dependencies`，不建立module graph，也不更改appup/relup。单module parse transform
无法知道某个Promise最终在哪个module定义。partial release窗口中callback若遇到暂时不兼容的current
owner，可以structured failure；application upgrade/downgrade本身不因此被sf-migrate阻止。

未来Rebar plugin可以独立做whole-project warning，但不得成为本change correctness依赖。

## 10. Required Tests

- migration-enabled owner缺失显式`-vsn`编译失败；literal Vsn exact match且不做term coercion；
- `sf_do_code_change/2` clauses/case选择多个exact historical Vsns；unknown source返回unhandled；
- complete historical named sf-do分析；partial/unnamed block拒绝；historical body不生成或执行；
- old/current StepName精确解析；rename mapping；missing Step/Block和duplicate source diagnostics；
- raw bridge仅在Outcome shape兼容且target captures为source subset时编译；
- converter同时转换Outcome/Env；same-identity converter覆盖automatic inheritance；
- converter error、invalid return、incompatible output与user exception边界；
- callback/frame不被重写，request不重发，Step前半段不重跑，direct parent_ref保持；
- 无plan但相同identity+Env subset在up/down都automatic resume；
- B→A时B Env只增加可恢复，删除A所需capture则reply-time failure；
- down Outcome原样传递且不调用reverse converter；
- renamed/incompatible down callback只failure自身，不阻止appdown；
- linked multi-owner frames逐node独立resolve，ordinary callback不变；
- no State scan、no callbacks offset API、no dependency attribute、no migration manifest compatibility reader；
- compile、EUnit、Common Test、xref、dialyzer与`openspec validate sf-migrate --strict`通过。
