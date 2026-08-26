# Initiative Context

## Binding

- Goal: `sf-cc`
- Generation: `1`
- Predecessor context: `context-(N-1).md | none`
- Predecessor disposition: `unavailable`
- Collected at: `2026-08-26T20:17:37+08:00`
- Evidence target commit/tree: `4d615c6e84bd380c016b3f97462deed0e8711ca5` / `03d252ed7a71e77ad9b71757981dd7721bcc0da1`
- Target before collection: `aab6356893524e0cb36293837a4c995d6a33769e` / `adad11209e224d298779ba53d1d9e187d5806a82`
- Target after collection: `4d615c6e84bd380c016b3f97462deed0e8711ca5` / `03d252ed7a71e77ad9b71757981dd7721bcc0da1`
- Currency: `current`
- Repository revisions: accepted merge `4d615c6e84bd380c016b3f97462deed0e8711ca5`（parents `aab6356893524e0cb36293837a4c995d6a33769e` and `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03`，tree `03d252ed7a71e77ad9b71757981dd7721bcc0da1`）；task-test-1 checkpoint `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03`（parent `aab6356893524e0cb36293837a4c995d6a33769e`，same tree）；prior accepted derived fix `aab6356893524e0cb36293837a4c995d6a33769e`（parent `bc28febe...`，tree `adad1120...`）；历史 coding merge `bc28febe...` 的 first parent 为 `9691772e...`、second parent/candidate 为 `7b9c28a...`，merge tree `8f682a2e...`
- Normative inputs: `openspec/changes/sf-cc/changelog.md`、`design.md`、`implementation.md`、`proposal.md`、`specs/sf-cc/spec.md`、`tasks.md`，以及 `openspec/changes/sf-do/changelog.md`
- Authorized history ranges: `9691772e7e260e32cb5c7fd11c0cd51563b3530a..bc28febe142000025abaf59c71c8b41ab6cffe99`；候选范围 `9691772e...7b9c28a...`；fix 范围 `bc28febe142000025abaf59c71c8b41ab6cffe99..aab6356893524e0cb36293837a4c995d6a33769e`；test checkpoint 范围 `aab6356893524e0cb36293837a4c995d6a33769e..d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03`；accepted merge 范围 `aab6356893524e0cb36293837a4c995d6a33769e..4d615c6e84bd380c016b3f97462deed0e8711ca5`；仅检查上述目标对象及其 merge/候选/fix/test-checkpoint 父关系
- Collection contract: `task-test-1` checkpoint `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03` 已合并至 accepted target `4d615c6e84bd380c016b3f97462deed0e8711ca5` 后刷新；独立检查 immutable before/after target、exact changed paths、capability/interface disposition、test merge evidence、report/review-3 closure、R15 public tag、9 deferred cases 与 available validation caveats；只更新本文件

## Predecessor delta

- Previous bindings: accepted fix target 为 `aab6356893524e0cb36293837a4c995d6a33769e` / `adad11209e224d298779ba53d1d9e187d5806a82`；控制工作树没有可读取的 predecessor `context-0.md`
- Generation delta: `task-test-1` checkpoint `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03` 从 `aab6356893524e0cb36293837a4c995d6a33769e` 派生并修改四个 test paths；accepted merge `4d615c6e84bd380c016b3f97462deed0e8711ca5` 将该 checkpoint 与 prior fix 合并，product source/interface 无 delta；retired production symbols：none
- Retained and rebound evidence IDs: E-CC-03 至 E-CC-07 的 target-bound normative/current-code claims 已在新 target 重新绑定；E-CC-08 至 E-CC-10 保留为历史/control evidence；E-CC-11 至 E-CC-13 保留 fix parent/path、public adapter 与 correction review evidence；新增 E-CC-15 至 E-CC-17 覆盖 test checkpoint/merge、report-3 与 review-3 closure
- Invalidated evidence IDs: E-CC-04 至 E-CC-07 的旧 `aab635689...` bindings 被新 target binding 替代；E-CC-14 的 moving-worktree test-delta exclusion 被 accepted checkpoint 合并事实取代；未删除 normative requirement 或历史基线
- Prior gaps addressed: `task-test-1-fix-1` 的 R15 public tag correction review passed；accepted merge 后 report-3/review-3 均 passed；R15 exact public tag、此前 9 个 deferred fixture/expectation failures、focused/full available validations 已关闭
- Unchanged searches not repeated: 未重复整个项目 API、测试或历史清单；只重新核验新 merge 的 parent/tree/changed paths、test checkpoint、report/review-3、R15 assertion 与 available-validation boundary

## Task-relevant normative requirement index

- R1–R2：专用 `run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5` 与 ordinary/wait runner compatibility；每个升级后 runner invocation 创建 CcRef。
- R3–R5：external-fun macro source/arity gate；descriptor 原样保存 callback 与 opaque Context；async/direct-state callback return contract。
- R6–R8：`#callback{}` 三/四元兼容、`sf_cc_mref` 传播、AccRef 与 CcRef 独立、唯一 CcRef final owner。
- R9–R10：preflight、take、owned resource cleanup 后才 invoke；exactly-once、take 后不 rollback/retry。
- R11–R14：message 不消费 completion；结构失败绕过不可用 continuation；terminal 只存 CcRef；旧 callback tuple 可读。
- R15–R17：unavailable old continuation 返回 `{legacy_outcome, Outcome, CleanState}`；inspect 只暴露 sanitized metadata；matcher preflight atomic。
- R18–R19：active fail 按 CcRef 去重并继续报告单个失败；frame/manifest 保存 literal Module/Vsn 并在每个 frame MFA 前独立 resolve。

## Brief project orientation

`async_m` 是 public facade 与 receive-loop adapter，现由显式 `handle_info/3` 在 public boundary 保留 R15 tagged outcome；`async_t` 管理 action callback、scope、路由与 runner，`async_r_t` 提供 reader/state scope；`async_util` 管理 callback dictionary/backend 与 decoder。新增 `async_sf_cc` 是 CcRef descriptor/store/completion/inspect/fail 的唯一 owner；`async_sf_runtime` 仅负责 stable frame execution、resolution 与向 completion owner 转发。`async_sf_transform` 产出 frame/manifest 的 owner Module/Vsn。该方向仅用于本 Goal 的 ownership、lifecycle、frame 和 downstream test handoff。

## Task-relevant capability map

| Capability | Target owner | 当前证据与 disposition |
|---|---|---|
| callback representation/decoder/classifier | `include/async_internal.hrl` + `async_util` | `#callback` 增加 `sf_cc_mref`；`decode_callback/1` 接受旧三元与新 record；`callback_value_kind/1` 区分 `sf_cc_ref`，替代旧旁路读取 |
| reader-local execution scope | `async_r_t` | 保留 AccRef，新增 CcRef reader/local helper；`async_t` 在 callback/action propagation 中同时携带两者 |
| final descriptor/CcRef/completion/inspection/fail | `async_sf_cc` | 新增唯一 owner；`prepare/5` 负责 take、owned cleanup、orphan accumulator 处理，之后 invoke |
| sf-prefixed source validation | `async_sf_cc_macro` + `include/async.hrl` | 独立 provider 注册三个 public macro，运行时仍接收原 external fun term |
| ordinary runners/wait/routing | `async_m` + `async_t` | facade 与 normal result semantics 保留；显式 public `handle_info/3` 仅对既有 R15 tag 保留完整 tagged result，升级后 action 进入 common CcRef lifecycle |
| stable frame completion | `async_sf_runtime` → `async_sf_cc` | retired embedded descriptor/invoker 被替换为 `sf_cc_ref` terminal 与 completion delegation |
| frame owner/version metadata | `async_sf_transform` + `async_sf_runtime` | frame/manifest 保存 owner Module 与 literal Vsn；resume 前执行 per-node resolution |
| callbacks backend/state utilities | `async_util` | 复用 map/orddict/list/dict primitives 与 state compatibility，扩展 callback classification |

## Task-relevant reusable interface inventory

- `async_m:run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5`（accepted target `src/async_m.erl:209-218`）：public facade；分别建立 async_r_t/direct-state execution，`exec` 只作 result projection。显式 `handle_info/3`（`:220-224`）对 `run_info/3` 的 R15 `legacy_outcome` pair 返回完整 tagged value，其他 pair 继续返回 state projection；`inspect_sf_callbacks/3`、`fail_sf_callbacks/4`（`:226-230`）转发给 `async_sf_cc`。
- `async_t:run_sf_cc/4`、`run_with_sf_cc/6`、`exec_sf_cc/6`（`src/async_t.erl:786-803`）：在 effect execution 时注册 CcRef、安装独立 scope，并令新 action callback 保存 `sf_cc_mref`。
- `async_sf_cc:new/3`、`new_ref/2`、`terminal/1`、`complete/3`、`complete_state/4`（`src/async_sf_cc.erl:20-87`）：descriptor/entry/terminal 与唯一 completion API；mode/arity guard 是 runtime protocol validation。
- `async_sf_cc:prepare/5`（`:120-178`）：entry presence 检查、CcRef take、同 owner action/timer/monitor/accumulator cleanup、CleanState 构造；entry 缺失返回 `unhandled`。这是 completion cleanup 的唯一 owner。
- `async_util:decode_callback/1`、`callback_exists/1`、`callback_value_kind/1`（`src/async_util.erl:75-96`）：旧 `{callback,Cc,AccRef}` 解码为 `sf_cc_mref = undefined`，record 读取新增字段；`callback_exists/1` 只统计 action callback。
- `async_r_t:get_local_ref/1`、`get_sf_cc_ref/1`、`local_ref/3`、`local_sf_cc_ref/3`（`src/async_r_t.erl:205-240`）：AccRef/CcRef 分离读写；`async_t` 的 continuation resume 通过该 scope 继承 CcRef。
- `async_sf_runtime:continue_valid/4`、`resolve_frame/2`、`complete_current/2`（`src/async_sf_runtime.erl:94-120,137-146,317-323`）：frame terminal 识别与 per-node resolve，structural/directional failure 进入 `async_sf_cc`；不再递归寻找嵌入 descriptor。
- `async_sf_transform:module_vsn/1`、manifest/frame construction（`src/async_sf_transform.erl:690-700,944-970`）：生成 owner Module 与 literal Vsn，供 reply-time resolution/inspection 使用。
- `async_sf_cc_macro` exports `run_sf_cc/3`、`run_with_sf_cc/5`、`exec_sf_cc/5`（`src/async_sf_cc_macro.erl:8-18`）：只接受规定 external-fun AST/arity，并保留 AST 交给普通 runtime call；编译器 diagnostic 属于 macro boundary。

## Task-relevant existing behavioral coverage

- `task-test-1-report-2` 在旧 accepted target `bc28febe...` 记录的 9 个 deferred fixture/expectation failures 与 test-owned coverage omissions 已关闭；当时唯一剩余 failure 是 R15 `old_three_tuple_unavailable`，runtime suite 9/10、full CT 63 passed/1 failed。
- accepted fix target `aab6356893524e0cb36293837a4c995d6a33769e` 的 `task-test-1-fix-1-review-1` verdict 为 `passed`：public probe 已观察到精确 `{legacy_outcome, Outcome, CleanState}`；resumable old tuple、duplicate event、business exception、nested `undef` negative probes，以及 compile、EUnit、xref 均由 supplied worker evidence 记录通过；diff 仅为 `src/async_m.erl`。
- fix review 明确：`async_t:run_info/4` 既有 runtime pair `{{legacy_outcome, Outcome, CleanState}, CleanState}` 由显式 `async_m:handle_info/3` 原样保留，普通 pair 继续返回 state projection；cleanup、classification、CcRef owner 未复制或改变。
- accepted test checkpoint `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03` 与 merge target `4d615c6e84bd380c016b3f97462deed0e8711ca5` 的 `task-test-1-report-3`、`task-test-1-review-3` 均记录 passed：`async_sf_cc_SUITE`、`async_sf_cc_macro_SUITE`、`async_sf_SUITE`、`async_t_SUITE` focused suites 及 full `rebar3 ct` 均通过；9 个 deferred cases 与 R15 exact tag 已关闭。
- 既有环境限制继续不作 pass claim：`rebar3 dialyzer` prior exit 1 with 140 product/dependency warnings；`npx openspec validate sf-cc --strict` executable unavailable；`rebar3 docker_ci run` 缺少 `erlang:21` image，OTP 21–29 matrix 不可用。

## Historical evidence

- 旧 target `9691772e...` 的历史 sf runtime 将完整 `sf_cc` descriptor 嵌入 frame terminal；accepted candidate 将其替换为 `#{kind => sf_cc_ref, protocol => 1, mref => CcRef}`，由 `async_sf_cc` 统一 completion。
- merge commit `bc28febe...` 有且仅有两个 parent：旧控制目标与 candidate；candidate `7b9c28a...` 的唯一 parent 为 `34e3a746...`。merge tree 已独立由 Git object 验证为 `8f682a2e...`。
- accepted test merge `4d615c6e84bd380c016b3f97462deed0e8711ca5` 有且仅有两个 parent：prior fix `aab6356893524e0cb36293837a4c995d6a33769e` 与 test checkpoint `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03`；merge tree `03d252ed7a71e77ad9b71757981dd7721bcc0da1`，相对 prior fix 仅合入四个 test paths。
- review-4 的 `S1`（direct-state 重复 cleanup path）已在 candidate review-5 精确关闭；review-5 verdict 为 `passed`，没有新增 product finding。
- `task-test-1-fix-1` 是 `bc28febe...` 的唯一子提交；其 `src/async_m.erl` delta 把 `handle_info/3` 从 generated facade function list 移出并提供显式 public adapter，以保留 R15 `legacy_outcome` tag。fix review verdict 为 `passed`，无 findings。
- `sf-do/changelog.md` 保留历史边界：stable frame owner Module/Vsn 与 lazy resolution 属于本 generation；historical block、migrator/catalog/composition/downgrade policy 仍由 `sf-migrate` 拥有，candidate 未实现这些越界能力。

## Conflicts, stale evidence, inferences, and unknowns

- 当前 control worktree 的 `docs/plan/sf-cc/` 下未跟踪文件是 pre-existing generation-bound workflow artifacts；按 assignment 明确归类为 `do-not-transfer` control artifacts，不是 product target，不纳入 changed-path transfer，也不请求提交；本文件仅为本次唯一授权写入。
- `context-1.md` 原先在 control worktree 不存在，因此 predecessor artifact unavailable；本文件以 supplied accepted contracts/reviews 作为 discovery index，并将实现事实重新绑定 immutable accepted fix tree。
- review-5 的 `passed` 仅覆盖 compile/EUnit/xref/diff-check 与 coding semantic review；它不覆盖两个 CT suite，也不代表 task-test acceptance。
- 由 fix review evidence 可推断 R15 public tag defect 已由 `task-test-1-fix-1` 修复并完成 review closure；不能由此推断 fix 后 task-test full rerun、OpenSpec strict validation、dialyzer 或 OTP compatibility matrix 已通过。
- 当前 moving worktree 仅有 assignment 指定的未跟踪 `docs/plan/sf-cc/` control artifacts；accepted test paths 已由 `d336fbf...` 合并进 current target，不再作为 provisional delta。控制文档仍不属于 product target。

## Context evidence request closure

- 本次 post-commit refresh request：`task-test-1` checkpoint `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03` accepted merge `4d615c6e84bd380c016b3f97462deed0e8711ca5`；状态 `closed`。
- 已关闭：immutable before/after target binding、merge parent/tree/changed-path relation、test-only capability/interface disposition、report-3/review-3 passed、R15 exact public tag、9 deferred cases、focused/full available validations 与 preserved caveats。
- 保持 open（环境边界，不是本 context 的产品修复）：Dialyzer prior 140 warnings、OpenSpec CLI unavailable、OTP 21–29 matrix/image unavailable；这些均不作 pass claim。

## Evidence index

### Evidence E-CC-01

- Claim: merge target、before target、candidate 与 tree 关系精确可验证。
- Evidence class: `history`
- Bound revision: `bc28febe142000025abaf59c71c8b41ab6cffe99`、`9691772e7e260e32cb5c7fd11c0cd51563b3530a`、`7b9c28a52b34f199ead7469d12c6124305853b35`
- Key search strings: `tree`, `parent`, `merge task-coding-1 checkpoint`
- Search method: `git cat-file -p`、`git rev-parse <rev>^{tree}`、`git merge-base`
- Search scope: supplied immutable Git objects only
- Location: merge commit object；target tree `8f682a2e...`；before tree `b19b8556...`
- Finding: merge commit first parent is old target, second parent is candidate；merge tree is `8f682a2e...`
- Limits: 不判断 control worktree 未跟踪文档是否可提交

### Evidence E-CC-02

- Claim: accepted merge 只改动 12 个 task-coding-1 owned product paths。
- Evidence class: `history`
- Bound revision: `bc28febe142000025abaf59c71c8b41ab6cffe99`
- Key search strings: `include/async.hrl`, `async_sf_cc`, `async_sf_runtime`, `async_t`, `async_util`
- Search method: `git diff --name-status`、`git show --stat`
- Search scope: `9691772e...bc28febe...`
- Location: merge diff path list
- Finding: `include/async.hrl`、`include/async_internal.hrl`、`rebar.config`、`src/async_m.erl`、`src/async_r_t.erl`、`src/async_sf_cc.erl`、`src/async_sf_cc_macro.erl`、`src/async_sf_runtime.erl`、`src/async_sf_transform.erl`、`src/async_t.erl`、`src/async_util.erl`、`src/monad_frame.erl`
- Limits: 不包含 test/OpenSpec/workflow artifacts；未将未跟踪 docs 误算入 merge diff

### Evidence E-CC-03

- Claim: R1–R19 与 sf-do changelog 的 ownership/lifecycle/frame boundary 是本 Goal 的 normative contract。
- Evidence class: `normative`
- Bound revision: `4d615c6e84bd380c016b3f97462deed0e8711ca5` / tree `03d252ed7a71e77ad9b71757981dd7721bcc0da1`（normative files unchanged from prior accepted target）
- Key search strings: `### Requirement`, `CcRef`, `sf_cc_ref`, `legacy_outcome`, `Module`, `Vsn`, `sf_cc_mref`
- Search method: `git show <target>:openspec/...`、`rg -n '^### Requirement|CcRef|sf_cc_ref|legacy_outcome'`
- Search scope: `openspec/changes/sf-cc/*.md`、`openspec/changes/sf-cc/specs/sf-cc/spec.md`、`openspec/changes/sf-do/changelog.md`
- Location: sf-cc spec requirements in order 1–19；sf-do changelog 2026-08-26 entry
- Finding: explicit external-fun runners、single CcRef owner、cleanup-before-invoke、old tuple fallback、sanitized matcher、Module/Vsn resolution；sf-migrate policy excluded
- Limits: 不把 OpenSpec checkbox 或旧 changelog 的 “尚未实现” 段落当作当前实现证据

### Evidence E-CC-04

- Claim: public runner/macro/descriptor interfaces在 accepted target 中存在并保持 mode distinctions。
- Evidence class: `current-code`
- Bound revision: `4d615c6e84bd380c016b3f97462deed0e8711ca5` / tree `03d252ed7a71e77ad9b71757981dd7721bcc0da1`
- Key search strings: `run_sf_cc`, `run_with_sf_cc`, `exec_sf_cc`, `new(async_r_t`, `new(direct_state`, `export_macro`
- Search method: `git show <target>:<path> | nl -ba | rg`
- Search scope: `include/async.hrl`、`src/async_m.erl:37-38,209-230`、`src/async_sf_cc.erl:20-43`、`src/async_sf_cc_macro.erl:8-18`
- Location: listed symbols and exact line ranges above；public `handle_info/3` at `src/async_m.erl:220-224`
- Finding: three sf-prefixed macro/runtime entry points registered/exported；descriptor保存 callback/context；direct mode接受 `/2|3`，async mode只接受 `/2`
- Limits: macro compiler diagnostics/full source-form matrix需由 task-test-1 直接验证

### Evidence E-CC-05

- Claim: CcRef completion path is linear and cleanup occurs before invocation；inspect/fail share owner。
- Evidence class: `current-code`
- Bound revision: `4d615c6e84bd380c016b3f97462deed0e8711ca5` / tree `03d252ed7a71e77ad9b71757981dd7721bcc0da1`
- Key search strings: `complete(`, `prepare(`, `remove_owned`, `remove_orphan_accumulators`, `invoke_prepared`, `commit_failures`
- Search method: immutable source inspection with numbered lines
- Search scope: `src/async_sf_cc.erl:64-87,89-108,120-178,180-214,263-303`
- Location: `async_sf_cc.erl` exact ranges above
- Finding: `prepare/5` performs entry validation/take, owned resource/action cleanup and orphan accumulator handling；`invoke_prepared/4` writes CleanState then invokes；CcRef absence is `unhandled`；batch fail deduplicates CcRefs and reports `cc_failed`
- Limits: source inspection不能替代 callback crash/late reply/duplicate completion black-box tests

### Evidence E-CC-06

- Claim: callback tuple compatibility and independent AccRef/CcRef scope are encoded in shared helpers。
- Evidence class: `current-code`
- Bound revision: `4d615c6e84bd380c016b3f97462deed0e8711ca5` / tree `03d252ed7a71e77ad9b71757981dd7721bcc0da1`
- Key search strings: `decode_callback`, `callback_exists`, `get_sf_cc_ref`, `local_sf_cc_ref`, `sf_cc_mref`
- Search method: `git show <target>:<path> | nl -ba | rg`
- Search scope: `include/async_internal.hrl:2-7`、`src/async_util.erl:75-96`、`src/async_r_t.erl:205-240`、`src/async_t.erl:429-434,868-896`
- Location: exact lines above
- Finding: old three-tuple decodes to undefined CcRef；new callbacks carry CcRef；scope helper preserves separate AccRef/CcRef；router uses shared decoder
- Limits: all backend and sequential/parallel propagation cases remain downstream test responsibility

### Evidence E-CC-07

- Claim: stable frame terminal/owner metadata/resolution now use CcRef-only terminal and Module/Vsn。
- Evidence class: `current-code`
- Bound revision: `4d615c6e84bd380c016b3f97462deed0e8711ca5` / tree `03d252ed7a71e77ad9b71757981dd7721bcc0da1`
- Key search strings: `sf_cc_ref`, `resolve_frame`, `resolve_current_manifest`, `module_vsn`, `vsn =>`, `complete_current`
- Search method: immutable source inspection with numbered lines
- Search scope: `src/async_sf_runtime.erl:94-120,137-146,224-279,305-323`；`src/async_sf_transform.erl:15-18,393-395,690-700,944-970`
- Location: exact ranges above
- Finding: terminal dispatch delegates to `async_sf_cc`；frame/manifest carries owner Module/literal Vsn；each current frame resolves callable MFA before invocation；structural/directional failure uses current CcRef scope
- Limits: nested multi-module, external upgrade and literal Vsn observable cases未由 coding review单独证明

### Evidence E-CC-08

- Claim: task-coding-1 accepted semantic review passed and S1 was closed。
- Evidence class: `test`
- Bound revision: candidate `7b9c28a52b34f199ead7469d12c6124305853b35`，merge target `bc28febe...`
- Key search strings: `Verdict: passed`, `S1`, `rebar3 compile`, `rebar3 eunit`, `rebar3 xref`
- Search method: supplied control artifact `docs/plan/sf-cc/task-coding-1-review-5.md`
- Search scope: review-5 claims about candidate and cumulative base diff
- Location: review-5 Evidence/Findings/Verdict/Caveats
- Finding: review-5 passed；compile/eunit/xref/diff-check passed；无新 product finding；candidate可进入 merge gate
- Limits: review artifact未证明下游 CT/full matrix；它是 control evidence，不是 target tree file

### Evidence E-CC-09

- Claim: exactly 9 deferred CT failures remain open and are owned by task-test-1；没有 CT pass claim。
- Evidence class: `test`
- Bound revision: candidate `7b9c28a52b34f199ead7469d12c6124305853b35` and accepted contract in control artifacts
- Key search strings: `same_monad_lowering:408`, `frame_completion:162`, `nested_composition:415`, `capture_compatibility:239`, `structural_failures`, `test_async_t_progn_par:495`, `test_async_t_pmap_multiple_messages:628`, `test_async_t_pmap:736`, `test_async_t_pmap_with_acc:797`
- Search method: supplied `task-coding-1-evidence-boundary-1.md`、`task-coding-1-review-5.md`、`task-coding-1.md`
- Search scope: accepted Deferred downstream validation clauses and review caveats
- Location: task contract `## Deferred downstream validation` and review-5 `Caveats`
- Finding: sf suite 5 cases是 retired terminal shape expectation；async_t suite 4 cases是 strict cleanup 后旧 accumulator/list expectation；owner=`task-test-1`
- Limits: 未运行或未重新运行 CT；不得把这 9 项写成 passed

### Evidence E-CC-10

- Claim: control docs are explicitly excluded dirty paths。
- Evidence class: `current-code`
- Bound revision: control worktree snapshot；不属于 immutable product target
- Key search strings: `?? docs/plan/sf-cc/`
- Search method: `git status --short --untracked-files=all`
- Search scope: all untracked paths under `docs/plan/sf-cc/`
- Location: `lanes-1.md`、`plan-1.md`、`sessions-1.yaml`、`status.md`、task/review/evidence artifacts；本文件为本次唯一新增 context artifact
- Finding: assignment 已声明这些是 pre-existing generation-bound workflow artifacts；分类为 `do-not-transfer`，不 stage/commit/复制/删除
- Limits: 不把 control-doc 内容当作 product source 或 target tree evidence

### Evidence E-CC-11

- Claim: accepted derived fix 的 parent/tree/path 关系精确可验证，且只改动 `src/async_m.erl`。
- Evidence class: `history`
- Bound revision: `aab6356893524e0cb36293837a4c995d6a33769e`
- Key search strings: `parent`, `tree`, `fix(async_m): preserve legacy outcome tag`, `src/async_m.erl`
- Search method: `git cat-file -p`、`git rev-parse <rev>^{tree}`、`git diff --name-status <parent> <rev>`、`git show --stat`
- Search scope: supplied immutable fix object及其唯一 parent `bc28febe...`
- Location: fix commit object；fix tree `adad1120...`；`bc28febe.....aab63568...` diff path list
- Finding: fix commit parent 为 accepted merge `bc28febe...`；tree 从 `8f682a2e...` 变为 `adad1120...`；changed path 只有 `src/async_m.erl`
- Limits: 不将 moving worktree 的 test/doc delta 计入 accepted fix

### Evidence E-CC-12

- Claim: accepted target 的 public `async_m:handle_info/3` 保留 R15 runtime tagged result，同时维持普通 state projection；generated facade 不再生成同名 `handle_info/3`。
- Evidence class: `current-code`
- Bound revision: `aab6356893524e0cb36293837a4c995d6a33769e` / tree `adad11209e224d298779ba53d1d9e187d5806a82`
- Key search strings: `-export([handle_info/3])`, `functions => [run_info/3`, `handle_info(Info, Offset, State)`, `legacy_outcome`, `run_info(Info, Offset, State)`
- Search method: `git show <target>:src/async_m.erl | nl -ba | rg`
- Search scope: immutable `src/async_m.erl` only
- Location: export `src/async_m.erl:38`；generated function list `:113`；adapter `:220-224`
- Finding: explicit adapter matches `{{legacy_outcome, _Outcome, _CleanState} = Tagged, _State}` and returns `Tagged`; all other pairs return `NewState`; no cleanup/classifier/runtime owner is added at this boundary
- Limits: source shape alone cannot establish full post-fix CT rerun or external OTP compatibility

### Evidence E-CC-13

- Claim: `task-test-1-fix-1` review passed and closed the R15 public tag defect before the subsequent task-test checkpoint。
- Evidence class: `test`
- Bound revision: candidate `aab6356893524e0cb36293837a4c995d6a33769e`，prior failure target `bc28febe...`
- Key search strings: `Status: complete`, `Verdict: passed`, `R15`, `public`, `full rerun`, `dialyzer`, `OpenSpec`, `OTP`
- Search method: supplied `docs/plan/sf-cc/task-test-1-fix-1-review-1.md` and fix contract/reports；review evidence audit
- Search scope: fix review Findings/Verdict/Caveats and task-test-1 resume gate
- Location: fix review `Evidence`/`Verdict`/`Caveats`；fix contract `Resume gate: rerun task-test-1`
- Finding: review verdict `passed` with no findings；public probe and preserved-invariant probes are recorded passed；at review time task-test-1 full rerun was pending and dialyzer/OpenSpec/OTP matrix were not passed
- Limits: supplied review is pre-merge control evidence；subsequent report-3/review-3 closure is recorded separately in E-CC-16/E-CC-17

### Evidence E-CC-14

- Claim: prior moving-worktree test delta was provisional before the accepted test checkpoint and is invalidated as current evidence by the checkpoint merge。
- Evidence class: `current-code`
- Bound revision: control worktree snapshot；不属于 immutable accepted target
- Status: `invalidated-by-accepted-checkpoint`
- Key search strings: ` M test/async_sf_SUITE.erl`, ` M test/async_t_SUITE.erl`, `?? test/async_sf_cc_SUITE.erl`, `?? test/async_sf_cc_macro_SUITE.erl`
- Search method: `git status --short --untracked-files=all`、`git diff -- test/async_sf_SUITE.erl test/async_t_SUITE.erl`
- Search scope: exact dirty paths reported by control worktree
- Location: `test/async_sf_SUITE.erl`、`test/async_t_SUITE.erl`、`test/async_sf_cc_SUITE.erl`、`test/async_sf_cc_macro_SUITE.erl`
- Finding: these paths were previously absent from the accepted fix tree, then were independently verified in `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03` and merged into `4d615c6e84bd380c016b3f97462deed0e8711ca5`
- Limits: historical worktree snapshot does not establish the accepted commit; E-CC-15 is the authoritative immutable changed-path evidence

### Evidence E-CC-15

- Claim: task-test-1 checkpoint is an immutable test-only delta merged into the accepted target；no product source/interface/capability path changed。
- Evidence class: `history`
- Bound revision: `4d615c6e84bd380c016b3f97462deed0e8711ca5`、`d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03`、prior `aab6356893524e0cb36293837a4c995d6a33769e`
- Key search strings: `tree`, `parent`, `test(sf-cc): close deferred fixtures and coverage`, `test/async_sf_SUITE.erl`, `test/async_t_SUITE.erl`
- Search method: `git cat-file -p`、`git rev-parse <rev>^{tree}`、`git diff --name-status <prior> <checkpoint>`、`git diff --name-status <prior> <merge>`、product-path zero-diff check
- Search scope: immutable accepted merge/checkpoint objects and exact range `aab6356893524e0cb36293837a4c995d6a33769e..4d615c6e84bd380c016b3f97462deed0e8711ca5`
- Location: merge object parents/tree；checkpoint object parent/tree；diff path list
- Finding: `4d615c6e84bd380c016b3f97462deed0e8711ca5` has parents `aab6356893524e0cb36293837a4c995d6a33769e` and `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03`；tree is `03d252ed7a71e77ad9b71757981dd7721bcc0da1`；only `test/async_sf_SUITE.erl`、`test/async_sf_cc_SUITE.erl`、`test/async_sf_cc_macro_SUITE.erl`、`test/async_t_SUITE.erl` changed；`src`、`include`、`rebar.config` are unchanged
- Limits: Git object inspection proves changed-path/disposition only, not command outcomes；those are supplied in E-CC-16/E-CC-17

### Evidence E-CC-16

- Claim: task-test-1 report-3 records R15 exact public tag, all four focused suites, full available validation, and all nine deferred cases as passed/closed。
- Evidence class: `test`
- Bound revision: checkpoint tree `03d252ed7a71e77ad9b71757981dd7721bcc0da1` via `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03` and accepted merge `4d615c6e84bd380c016b3f97462deed0e8711ca5`
- Key search strings: `old_three_tuple_unavailable`, `{legacy_outcome, purged_reply, PurgedClean}`, `10/10`, `3/3`, `25/25`, `64/64`, `The nine original deferred failures remain closed`, `Unavailable or not-passed checks`
- Search method: supplied control artifact `docs/plan/sf-cc/task-test-1-report-3.md` with numbered-line inspection；immutable target test source assertion inspection
- Search scope: report-3 lines 3–54；accepted target `test/async_sf_cc_SUITE.erl:182-191` and four owned test paths
- Location: report-3 `## Result`/`## Validation`/`## Coverage`/`## Unavailable or not-passed checks`/`## Closure`；R15 assertion at `test/async_sf_cc_SUITE.erl:188-191`
- Finding: focused results are `async_sf_cc_SUITE` 10/10、`async_sf_cc_macro_SUITE` 3/3、`async_sf_SUITE` 10/10、`async_t_SUITE` 25/25；compile、EUnit 12/12、full CT 64/64、xref、diff-check exit 0；R15 exact `{legacy_outcome, purged_reply, PurgedClean}` passed；9 deferred cases remain closed
- Limits: report-3 control artifact names pre-merge candidate `aab635689...`; checkpoint/merge Git tree is independently bound above；two existing unnamed `sf_do` warnings remain

### Evidence E-CC-17

- Claim: task-test-1 review-3 passed with no findings and confirms report-3 closure plus R1–R19 observable coverage。
- Evidence class: `test`
- Bound revision: checkpoint tree `03d252ed7a71e77ad9b71757981dd7721bcc0da1` via `d336fbf2c0d9117d912c1c8c56d1a41bf6d21f03` and accepted merge `4d615c6e84bd380c016b3f97462deed0e8711ca5`
- Key search strings: `Status: passed`, `Verdict`, `none`, `10/10`, `3/3`, `25/25`, `64/64`, `140`, `OpenSpec`, `OTP`
- Search method: supplied control artifact `docs/plan/sf-cc/task-test-1-review-3.md` with numbered-line inspection
- Search scope: review-3 lines 3–51；report-3 referenced by review
- Location: review-3 `## Evidence`/`## Findings`/`## Verdict`/`## Caveats`
- Finding: review verdict `passed`、Findings `none`；all owned coverage, 9 deferred cases, R15 exact return and available focused/full commands are accepted；Dialyzer 140 warnings, unavailable OpenSpec CLI and OTP matrix remain explicit caveats
- Limits: review-3 is supplied control evidence and does not convert unavailable/non-passing checks into pass claims

## Coverage limits

- Relevance boundary: 仅覆盖 `sf-cc` post-commit refresh、task-test-1-fix-1 accepted fix、R15 public adapter closure、CcRef/runner/frame ownership 与 task-test-1 resume gate
- Included: immutable before/after target/tree binding；accepted merge 的 12 paths 作为历史基线；fix 的 single owned path；R1–R19 relevant interfaces；review-5 与 fix review passed；supplied focused evidence；R15 closure；9 deferred test-owned failures 的 report-2 closure
- Excluded: `sf-migrate` implementation/policy；后续 test implementation 或新 correction；OpenSpec checkbox maintenance；全项目 API/测试地图；moving worktree claims；不把 control 文档的 candidate 字段当作 accepted Git target
- Unavailable: `rebar3 dialyzer` prior exit 1 with 140 product/dependency warnings、strict OpenSpec validation 因 CLI unavailable、OTP 21–29 compatibility matrix / `rebar3 docker_ci run` 因 required images（包括 `erlang:21`）缺失；均不作 pass claim
- Invalidated evidence IDs: E-CC-04 至 E-CC-07 的旧 `aab635689...` current-code bindings 已重绑至 `4d615c6...`；E-CC-14 的 moving-worktree test-delta exclusion 被 accepted checkpoint 合并事实取代；E-CC-08/E-CC-09/E-CC-13 保留为 historical/control evidence；未宣称 predecessor artifact 可用
- Provisional or concurrent worktree delta excluded from evidence: 任何未被 `4d615c6e84bd380c016b3f97462deed0e8711ca5` tree 包含的 delta；当前仅有 `docs/plan/sf-cc/` control artifacts，accepted 四个 test paths 采用 immutable Git object 与明确 supplied report/review claims
- Dirty paths classified unrelated: `docs/plan/sf-cc/lanes-1.md`、`plan-1.md`、`sessions-1.yaml`、`status.md`、`task-coding-1*.md`、`task-test-1*.md` 及相关 workflow artifacts：assignment 明确为 pre-existing、generation-bound、`do-not-transfer` control artifacts；本 context artifact 是本次唯一授权写入
- User-declared unrelated/not-to-commit paths: assignment 明确指定上述 control docs pre-existing、generation-bound、excluded from product target；不请求提交，不写入其内容。本次不 stage/commit 任何 dirty path
