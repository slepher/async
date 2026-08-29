# Status:

changes_required

# Task:

task-coding-2

# Reviewed commit:

`00528899cea8609b35b6746376fdcf250e5a5286`

# Evidence:

- 本轮严格复核 review-5 的 direct/failure dispatch correction，比较范围为上一 immutable candidate `6ad45d9e404958666a1fefefbcbdeaac3894df1d` 到当前 immutable candidate `00528899cea8609b35b6746376fdcf250e5a5286`，并只沿该增量检查此前 Findings 1–5、6–8 的相关不回归。
- coding worktree 的 HEAD 精确为 `00528899cea8609b35b6746376fdcf250e5a5286`，`git status --short` 为空，且 `6ad45d9e404958666a1fefefbcbdeaac3894df1d` 是其祖先。range 只含 commit `0052889 fix: preserve direct and failure dispatch ordering`，仅修改 task-owned `src/async_sf_cc.erl`，共 17 insertions / 10 deletions；没有 test、OpenSpec、plan、status 或 lane 变更，range `git diff --check` 通过。
- supplied independent validation中compile、xref、`eunit --module=async_sf_transform_tests`、diff-check、dialyzer均exit 0；`async_t_SUITE`为23 passed / 4 accepted AccRef legacy failures，未报告新signature；coding worktree clean。
- normal async mixed shared-parent保持闭合：`src/async_sf_cc.erl:317-328`只允许success进入pending preservation；`:289-304`的`async_r_t`分支先执行完整`preflight_node_in_store/3`再调用原outer collector。immutable beam最小复现得到pending ordinary `{{linked,ordinary}, StateWithSibling}`，移除最后child后stable为`{{linked,stable}, CleanState}`。
- failure ordering已恢复：failure不再匹配`:317`的success preservation，`:265-287`因此进入`take_node/4`；`:354-364`完成preflight、outer take与`cleanup_owned/3`，随后`:306-315`先put CleanState再forward/invoke。focused probe得到首次failure `{{error,boom},{state,#{}}}`，callback看到outer/sibling均不存在，重复failure返回`{unhandled,{state,#{}}}`。
- direct callback时点已部分闭合：合法pending child下，direct `/2`与`/3`均未执行；最后child移除后均只执行一次，`/3`观察到outer与sibling均不存在，最终state为空。但`:289-304`的`direct_state`分支还存在一个与review-5精确要求冲突的返回/preflight残差，见Finding 1。
- bounded omission pass：本range未改变representation、message non-consumption、AccRef post-result cleanup、action timer/index、single demonitor owner、classifier、availability/exception、manifest/transform identity或explicit Context路径；没有新增dispatcher、representation、catch、resource owner或retired caller。因此 Findings 1–2、4、6–8及Finding 5既有action-index/timer/demonitor子项未见增量回归；本轮残差局限于既有`dispatch_pending_parent/7`的direct分支。

# Findings:

1. **阻断：normal pending `direct_state` 虽已停止 callback，却绕过结构preflight并把当前outcome改写为`unhandled`。**
   - Exact path/symbol：`src/async_sf_cc.erl:289-304`，`dispatch_pending_parent/7`的`direct_state`分支；其`:291-292`直接执行`async_r_t:return(unhandled, MR)`，没有使用已经传入的`Ref`、`Node`、`Outcome`、`State`与`Get`完成preflight或保留outcome。
   - Contract basis：review-5 Finding 1的bounded correction明确要求normal pending `direct_state`“只preflight并保留state/返回当前outcome，不调用direct callback”，最后child移除后才走canonical take→CleanState→invoke；该要求同时维持accepted preflight-before-consume/error boundary以及ordinary结果兼容性。
   - Observed vs required：合法pending direct探针中callback次数确为0且state/resources unchanged，但结果是`{unhandled, OriginalState}`，而required是`{CurrentOutcome, OriginalState}`。更重要的是，构造shape正确但指向missing child的reverse index时，observed仍是`{unhandled, OriginalState}`；`preflight_node_in_store/3`本应经`preflight_children_direct/3`报告`{missing_child, OuterRef, ChildRef}`，required是在任何callback/consume前暴露`invalid_continuation`，同时保持原state/resources。
   - 已闭合部分：最后child移除后canonical take仍生效；direct `/2`只调用一次，direct `/3`只调用一次且收到无outer/sibling的CleanState。failure pending sibling也已canonical take/cleanup/forward，重复dispatch为unhandled。
   - Bounded correction route：Correction owner为`task-coding-2`，只修改`src/async_sf_cc.erl`的`dispatch_pending_parent/7` direct分支。与相邻`async_r_t`分支共享同一`preflight_node_in_store(Ref, Node, Get(State))`成功/错误边界；成功时仅`async_r_t:return(Outcome, MR)`，不得callback、take或改变state，错误时沿现有`{invalid_continuation, Reason}`边界抛出。不要增加新dispatcher、representation、cleanup owner或test-owned兼容分支。
   - Closure evidence：合法pending direct `/2|3`应返回原outcome、callback count 0且state/resources bit-for-bit unchanged；malformed/missing-child metadata应在callback/consume前error且state/resources unchanged；最后child后callback count 1且`/3`收到CleanState；normal async mixed与failure canonical focused evidence继续通过，并重跑既有blocking matrix及`async_t_SUITE`精确结果。
   - Remaining risk：合法ordinary direct结果目前会被错误降级为`unhandled`；malformed pending ownership metadata又被静默保留，既可能改变上层ordinary行为，也会推迟结构错误并留下不可收敛lifecycle state。
   - 是否可 merge：否。

2. **review-5 的 callback时点、normal async mixed与failure canonical take主体已闭合，其他 Findings 未见增量回归。**
   - 证据：focused probes分别证明async pending sibling保留并最终linked/clean、direct `/2|3`不提前调用且最终恰好一次、failure先清理outer/sibling后调用且重复unhandled；exact delta没有触及 Findings 1–2、4、6–8或Finding 5既有resource ownership实现。
   - Remaining risk：上述direct pending返回/preflight残差关闭前，不能将这些已通过子项外推为review-5 Finding 1整体闭合。
   - 是否可 merge：否，受Finding 1阻断。

# Verdict:

changes_required

# Changes:

- Correction owner：`task-coding-2`。
- 仅修正`src/async_sf_cc.erl`中`dispatch_pending_parent/7`的`direct_state`分支：先执行现有完整node/owned metadata preflight；成功时返回原`Outcome`并原样保留state，不执行callback；失败时沿现有`invalid_continuation`边界退出。
- 不修改tests、OpenSpec、plan、status或lanes，不重开已闭合的normal async mixed、failure canonical take以及 Findings 1–5、6–8的其他子项。
- 当前candidate不是merge-ready；新immutable checkpoint关闭上述单点残差并提供focused closure及既有validation后方可merge。

# Caveats:

- 本轮没有重新运行supplied validation matrix；命令结论采用用户提供的independent evidence。额外证据来自对immutable candidate已编译beam的只读focused probes，以及exact source delta的静态ordering检查。
- `async_t_SUITE`四项AccRef legacy failures仍属于既有accepted downstream transition，由`task-test-2`关闭；本review没有扩展accepted signature。
- 主worktree存在既有plan/status相关变更，本轮未触碰；未修改source、tests、OpenSpec、plan、status或lanes，未stage、commit、merge或delegate。唯一写入是本review artifact。
