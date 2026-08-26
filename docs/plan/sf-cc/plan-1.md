# sf-cc 实施计划

- Plan status: accepted
- Context: context-1.md
- Goal: `sf-cc`
- Generation: `1`
- Evidence target: `9691772e7e260e32cb5c7fd11c0cd51563b3530a`
- Normative inputs: `openspec/changes/sf-do/changelog.md` 与 `openspec/changes/sf-cc/` 下六个 Markdown 文档

## 目标与验收语义

本 generation 将 `sf-do` changelog 中的 final-completion 所有权变更与 `sf-cc` 的完整合同作为一个实施单元：所有升级后 public runner 都创建一次性 CcRef；legacy anonymous continuation 与 stable frame continuation 保留各自 resume 形式，但共同使用独立 final owner、cleanup-before-invoke 与 exactly-once completion。

完成必须同时满足以下可观察结果：

1. `run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5` 只接受文档规定的显式 external-fun source form 与 arity，descriptor 保存原 external fun、opaque Context 和 mode。
2. `wait/1`、`wait_t/2`、`run_cc/2`、`run_with_cc/4`、`exec_cc/4`、`run/4` 的公开参数、normalization、正常返回保持不变，但升级后新建 action 均进入 CcRef lifecycle。
3. action callback 统一兼容三元/四元 tuple；旧三元 tuple 永不伪造 CcRef。只有窄分类确认 old code 不可进入时，`async_m:handle_info/3` 才返回原 action input 的 `{legacy_outcome, Outcome, CleanState}`。
4. final completion 在用户代码前完成 CcRef take、同 owner action/timer/monitor/accumulator 清理；callback crash、非法返回、迟到 reply 或重复 fail 均不恢复或重试 CcRef。
5. `AccRef` 与 CcRef 独立传播；sequential suspension、local accumulator rebinding 和 parallel aggregation 不改变同一 Promise 的 final owner。
6. stable frame terminal 仅保存 `sf_cc_ref`；`#callback.sf_cc_mref` 是 structural failure 的直接 fallback。frame/manifest 保存 owner Module 与 literal module Vsn，resume 在调用每个 frame MFA 前独立执行 owner-resolution 边界。
7. inspect/fail matcher 只见 sanitized metadata；matcher preflight 原子，active fail 按 CcRef 去重并继续报告单个 callback failure。
8. ordinary message/reply/timeout/DOWN、source identity、capture 与 nested Monad 的既有合同保持不变；当前 10 个 sf-do CT 基线必须继续通过（仅将旧 terminal shape 断言迁移为新 shape）。

## 规范协调与边界

- `sf-cc` 是 final descriptor、CcRef entry、completion primitive、inspection/failure 的唯一 owner；`sf-do` 不再保留完整 descriptor、public runner 或 final invoker 的平行实现。
- `sf-do` 的 owner Module/Vsn 与逐节点 lazy resolution 边界属于本计划，因为它们决定 sf-cc summary 与 reply-time frame invocation；historical block、Env/Input migrator、migration catalog、version-chain composition、strict downgrade policy 仍由后续 `sf-migrate` 拥有。
- Context 不作 Env，不自动迁移、不向 matcher 暴露；CcRef 不是多个业务 Promise 的 operation-group identity。
- 不为用户 callback 外部副作用提供 rollback/retry，不把 anonymous final callback 提升为 external-fun 的跨 purge 保证。
- `sf-migrate` 的实现与文档未纳入本 generation；本计划只提供其未来 terminal failure 所需的稳定 `#callback.sf_cc_mref` 与 `async_sf_cc:complete` 边界。
- `openspec/changes/*/tasks.md` 的 checkbox 不作为完成证据，也不由 coding/test worker 修改；后续 dispatcher 只在相应验证门槛通过后机械维护。

## Capability disposition

| Capability | 当前 owner | 决策 | 最终 owner 与退役要求 | 证据 |
|---|---|---|---|---|
| callback representation/decoder/classifier | `async_t` 与 `async_util` 的重复 record/pattern | replace | shared internal header + `async_util` decoder/classifier；退役直接两字段 record pattern | E-10、E-12、E-13；R6、R8、R14 |
| reader-local execution scope | `async_r_t` raw AccRef | extend | `async_r_t` 兼容 raw Ref 并拥有 `{acc_ref, sf_cc_mref}` scope helper | E-14；R7 |
| final descriptor、CcRef store、completion、inspection/fail | 不存在；frame 内嵌旧 owner | new | 新 `async_sf_cc` 是唯一 runtime owner | E-15、E-20；R4、R8-R10、R16-R18 |
| sf-prefixed source validation | 不存在 | new | 独立 `async_sf_cc_macro`，由公共 include 启用 | E-17、E-20；R3 |
| ordinary runners、wait、routing | `async_m`/`async_t` | extend | 保留 facade/result semantics，内部委托 common CcRef core | E-11、E-12；R1、R2、R11、R15 |
| stable frame completion | `async_sf_runtime` 的完整 `sf_cc` terminal/invoker | replace | terminal 仅保留 CcRef；completion 委托 `async_sf_cc`；退役 `invoke_sf_cc` 与递归 terminal owner lookup | E-15、E-18；R12、R13 |
| frame owner/version metadata | `async_sf_transform` manifest/frame（无 Module/Vsn） | extend | producer 保存 owner Module/literal Vsn，runtime 提供逐节点 resolution 与 sanitized summary | E-16；R16、R19；sf-do changelog |
| State/backend utilities | `async_util` store/find/remove 与 `same_type_state/2` | reuse + extend | 复用 State compatibility 与三 backend primitives；扩展 classification，不新建第二套 dictionary abstraction | E-13 |

## 实施顺序理由

先统一 callback 编码与 reader scope，因为后续 CcRef entry、runner propagation、frame fallback 都依赖它们；随后建立唯一 completion primitive并冻结 cleanup/consume semantics；再迁移 public runners，使所有 post-upgrade action 都能产生可用的 protected callback；最后替换 sf-do terminal、增加 Module/Vsn/resolution、完成 matcher 与 legacy failure 边界。独立测试阶段只在 coding candidate 完整合并后开始，避免把尚未稳定的内部 shape 固化成测试设计。

## task-coding-1

一个 hard coding parent 完成整个 `sf-cc` change 的产品实现与 sf-do 集成。它按下列内部序列执行，但保持单一 candidate、单一共享生命周期 owner 和单一 review boundary：

1. shared callback encoding、三元/四元 decoder/classifier 与 backend-safe `callback_exists`；
2. backward-compatible async scope，确保 AccRef/CcRef 独立；
3. 新 `async_sf_cc` descriptor/store/completion primitive，先冻结 take/cleanup/invoke 和错误语义；
4. new macro/provider 与三个 sf-prefixed runtime facade；
5. ordinary runners/wait 迁入 common core，验证 public behavior 不变；
6. sf-do frame terminal/structural failure、owner Module/Vsn 与逐节点 resolution 边界；
7. inspect/fail 与 old unprotected tuple `legacy_outcome`；
8. 静态检查及现有回归自测，确认没有双 final owner、旧 record reader 或 CcRef retry path。

该父任务不拆 OpenSpec topology，也不把顺序步骤交给彼此独立的 worker，因为 callback store、scope、cleanup、runner normalization 与 frame fallback 共享同一个跨模块 exactly-once invariant。

## task-test-1

一个 hard document-driven test parent 在 coding candidate 后独立验证 R1-R19 和 sf-do changelog 的混合合同。它新增真正的 old/new tuple、protected/unprotected、macro、runner、frame、matcher、cleanup 与 backend fixtures；复用现有 suites 只验证回归，不重复把现有 ordinary behavior 当作新 sf-cc 证明。

验收以 observable behavior、State before/after、callback invocation count、compiler diagnostics、public result 与 privacy boundary 为准，不以内部 helper 调用或 OpenSpec checkbox 为准。完整门槛包括 compile、EUnit、Common Test、xref、dialyzer、支持的 OTP compatibility matrix，以及 OpenSpec strict validation。

## 完成定义

- coding 与 test contract 的全部 observable completion 条件满足，独立 review 通过；
- test report 绑定不可变 candidate/merge commit，且覆盖 R1-R19 与 sf-do 混合约束；
- 全部相关普通与 sf-do 基线行为无回归；
- 没有并行 final owner、未分类 callbacks dictionary value、未消费后 retry、隐私泄露或 sf-migrate 越界实现；
- 后续 dispatcher 才可根据证据维护 OpenSpec checklist、完成 merge 与宣布 initiative complete。
