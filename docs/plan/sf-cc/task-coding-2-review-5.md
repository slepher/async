# Status:

changes_required

# Task:

task-coding-2

# Reviewed commit:

`6ad45d9e404958666a1fefefbcbdeaac3894df1d`

# Evidence:

- 本轮是针对 task-test-2 暴露的 mixed ordinary/stable shared-parent defect 的 correction review。比较范围严格为已通过 review-4 的 `68a8fb708b7c1c93c881ce8569775a4ab866c55a` 到 immutable candidate `6ad45d9e404958666a1fefefbcbdeaac3894df1d`，并只沿该增量检查 Findings 1–8 的相关不回归。
- coding worktree 的 HEAD 精确为 `6ad45d9e404958666a1fefefbcbdeaac3894df1d`，`git status --short` 为空，且 `68a8fb708b7c1c93c881ce8569775a4ab866c55a` 是其祖先。range 只含 commit `6ad45d9 fix: preserve pending shared outer children`，仅修改 task-owned `src/async_sf_cc.erl`，共 50 insertions / 13 deletions；没有 test、OpenSpec、plan、status 或 lane 变更。
- supplied public reproduction已从 ordinary reply 后 stable `{ok,unhandled}` 恢复为 stable `{ok,{linked,stable}}`，最终state clean；ordinary完成时shared outer及pending stable sibling未被提前take/cleanup。supplied independent validation中compile、xref、`eunit --module=async_sf_transform_tests`、diff-check、dialyzer均exit 0；`async_t_SUITE`为23 passed / 4 accepted AccRef legacy failures，未报告新的mixed-parent signature。
- exact delta在`src/async_sf_cc.erl:265-321`增加`preserve_outer_with_children/3`：只要value分类为outer且reverse index有任意pending child，`dispatch_mode/4`就跳过`take_node/4`，经`dispatch_pending_parent/7`直接调用`execute_node/4`。该判定不区分outer mode，也不区分success/failure dispatch。
- Findings 1–2、4、6–8的representation、message route、AccRef cleanup、classifier、availability/exception、manifest及explicit Context路径没有被本range修改。Finding 5的action-index/timer/demonitor correction也保持不变；mixed normal async reproduction的shared-parent cleanup缺口已被定位并修复。
- final bounded omission pass发现一个direct-state与failure-order regression。对immutable candidate已编译beam执行只读最小复现：构造合法`direct_state` outer、一个pending ordinary child及其reverse index，调用`async_sf_cc:dispatch/3`；direct `/3` callback观察结果为`{seen,true,true}`，即传入state仍同时含outer与child。复现后coding worktree仍clean。

# Findings:

1. **阻断：pending-child preservation 对所有outer mode与failure dispatch生效，导致direct callback在outer未take时执行。**
   - Immutable evidence：`src/async_sf_cc.erl:265-287`在`preserve_outer_with_children/3`为true时绕过`take_node/4`；`:289-299`随后直接调用`execute_node/4`。`:312-321`只检查`outer_node`与non-empty children，不检查node的`mode`或dispatch的`Mode`。`execute_node/4`在`:635-636`把所有outer交给`invoke_outer/3`，其中`:706`后的`direct_state`分支从当前MR读取state并调用direct `/2|3` callback。
   - Contract basis：accepted Finding 3/5及task invariant要求normal/failure dispatch遵守preflight→take→direct cleanup→CleanState→invoke/forward；direct `/3` callback只可看到CleanState，direct `/2` side effect也只能在outer被消费后执行一次。shared parent可以在pending sibling期间保留，但不能由此把generic direct/failure executor变成non-consuming多次调用。
   - Observed vs required：supplied async mixed path现已保留sibling；但observed direct `/3` callback在outer与child仍pending时执行并看到二者，direct `/2`同样可提前/重复产生side effect。由于`dispatch_failure/3`也走相同preserve判定，failure outcome在pending child存在时同样跳过take/cleanup。required是pending shared-parent修正不改变direct CleanState、exactly-once及failure take-before-forward边界。
   - Independent reproduction：在合法callbacks store中放入`direct_state` outer、ordinary child与`?CHILDREN(Outer)`，运行exported `async_sf_cc:dispatch/3`；callback回报`{seen,true,true}`。这直接证明validation matrix与supplied mixed async reproduction未覆盖direct-state边界。
   - Bounded correction route：Correction owner为`task-coding-2`，allowed path仅需`src/async_sf_cc.erl`。将pending-parent处理按dispatch mode与outer mode明确分流：normal `async_r_t` shared-parent collector可保留现有non-consuming aggregation；normal `direct_state`在仍有children时只preflight并保留state/返回当前outcome，不调用direct callback，待最后child移除后走canonical take→CleanState→invoke；failure dispatch不得走pending success preservation，继续canonical take/cleanup/forward。不得增加第二种parent representation、root scan或silent child cleanup。
   - Focused closure：保留现有mixed ordinary/stable async public reproduction；新增production-equivalent direct `/2` callback count为1、direct `/3` callback仅在最后child后执行且收到不含outer/siblings的CleanState；failure with pending sibling证明outer先take、direct resources清理且不会重复forward。重跑完整blocking matrix与`async_t_SUITE`，不得出现新signature。
   - Remaining risk：在修正前，direct callback可观察/返回包含pending lifecycle metadata的state并产生重复side effect，failure path也可保留本应consume的outer，破坏Findings 3/5已经通过的ordering与ownership语义。
   - 是否可 merge：否。

2. **mixed ordinary/stable normal async defect本身已关闭，其他 Findings 未见增量回归。**
   - 证据：supplied exact public reproduction得到ordinary后stable `{ok,{linked,stable}}`及最终clean state；delta未触及message non-consumption、AccRef post-result cleanup、exact records/classifier、availability boundary、manifest/identity或explicit Context；action-specific `?CHILDREN(Key)`拒绝、timer唯一representation及single demonitor owner保持原实现。
   - Remaining risk：上述blocking direct/failure分流关闭前，不能把局部mixed-path通过外推为Findings 1–8整体不回归。
   - 是否可 merge：否，受Finding 1阻断。

# Verdict:

changes_required

# Changes:

- Correction owner：`task-coding-2`。
- 只修改`src/async_sf_cc.erl`的pending shared-parent dispatch分流：保留normal async mixed-parent修复，同时恢复direct `/2|3` callback的take/CleanState/exactly-once边界和failure dispatch的canonical take顺序。
- 不修改tests、OpenSpec、plan、status或lanes，不重开Findings 1–2、4、6–8及Finding 5中已通过的action-index/timer/demonitor correction。
- 新immutable checkpoint应提供`6ad45d9..new` delta、mixed async + direct `/2|3` + failure focused evidence、完整blocking self-validation和`async_t_SUITE`精确结果；上述边界全部通过后才可merge。

# Caveats:

- 本轮没有重新运行supplied validation matrix；命令结论采用用户提供的independent evidence。唯一额外执行是针对immutable candidate已编译beam的只读direct-state pending-child最小复现。
- `async_t_SUITE`四项AccRef legacy failures仍由既有downstream owner`task-test-2`关闭；本review没有扩展accepted signature。
- 本轮未修改source、tests、OpenSpec、plan、status或lanes，未stage、commit、merge或delegate；唯一写入是本review artifact。
