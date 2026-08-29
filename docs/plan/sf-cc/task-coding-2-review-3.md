# Status:

changes_required

# Task:

task-coding-2

# Reviewed commit:

`b380f21a8016143d4c836b240342c478823ba63d`

# Evidence:

- 本轮严格为 review-2 Finding 5 的 correction-only review，只审查 immutable range `cb03fb4c15cb55be438cc316308215bdbc5f7ca7..b380f21a8016143d4c836b240342c478823ba63d`，不重开 Findings 1–4、6–8。
- coding worktree 的 HEAD 精确为 `b380f21a8016143d4c836b240342c478823ba63d`，`git status --short` 为空，且 `cb03fb4c15cb55be438cc316308215bdbc5f7ca7` 是其祖先。range 只含 commit `b380f21 fix: harden cleanup ownership preflight`，只修改 `src/async_sf_cc.erl` 与 `src/async_t.erl`，共 48 insertions / 31 deletions；没有 test、OpenSpec、plan、status 或 lane 变更。
- supplied independent validation：compile、xref、`eunit --module=async_sf_transform_tests`、diff-check、dialyzer 均 exit 0；`async_t_SUITE` 为 23 passed / 4 accepted AccRef legacy failures；supplied focused evidence报告 reverse-index、duplicate timer、single demonitor owner三项通过。
- 静态 correction trace确认 ordinary timer由 `{sf_cc_timer, Key}` sidecar唯一承载，stable timer由 `#sf_callback.timer_ref`唯一承载；重复注册在 `send_after/3` 前返回 `duplicate_timer`。bounded omission search确认 affected production paths中唯一 `erlang:demonitor/2` 位于 `async_sf_cc:cleanup_request/1`，`async_t:demonitor_ref/2` 已删除。
- 但 current action reverse-index 的 empty-index 边界未被 supplied focused evidence覆盖。对 immutable candidate已编译 beam执行只读最小复现：给合法 ordinary action附加 `#{kind => sf_reverse_index, protocol => 1, children => []}`，调用 `async_sf_cc:route_action(Key, reply, Callbacks)`，结果为 `accepted_empty_action_index=true retained_after_consume=true`。该复现没有修改 worktree，复现后 worktree仍 clean。

# Findings:

1. **Finding 5 仍未完全闭合：action-owned empty reverse-index会绕过 preflight并在 consume 后残留。**
   - Immutable evidence：`src/async_sf_cc.erl:335-347` 的 `preflight_action_in_store/3` 新增调用 `preflight_children_direct/3`；但后者在 `src/async_sf_cc.erl:407-431` 对任何 shape-valid `children := []` 返回 `{ok, []}`，只有 non-empty action children才返回 `{error, {action_has_children, Ref}}`。`take_action_value/3`（`:552-556`）删除 action、ordinary timer sidecar与parent edge，却不删除 `?CHILDREN(Key)`。
   - Contract basis：review-2 Finding 5 的精确 correction要求 current action不存在任何 reverse-index/children，malformed ownership metadata必须在 consume和任何 resource side effect前拒绝，失败时 state/resources bit-for-bit unchanged；`async_sf_cc`必须保持 reverse-index与cleanup的单一 owner。
   - Observed vs required：observed 是 action下的 empty reverse-index被视为合法，reply consume action后该 namespaced key仍存在；required 是 action永远不是child owner，任何 action-owned reverse-index（包括empty set）都在 preflight阶段拒绝，action、timer、monitor、parent edge与完整 callbacks保持不变。
   - Why supplied focused evidence is insufficient：non-empty children、invalid index shape或missing child通过并不能证明empty-index边界；上述 immutable beam复现直接证明该边界仍失败。compile/xref/EUnit/Dialyzer通过也不能替代 ownership语义。
   - Bounded correction route：Correction owner 为 `task-coding-2`，只需修改 `src/async_sf_cc.erl`。在 `preflight_action_in_store/3` 使用 action-specific检查，要求 `async_util:find(?CHILDREN(Key), Callbacks) =:= error`；不要复用允许node empty child set的通用结果，也不要在 take 后静默删除该 malformed index。保持 timer representation、single demonitor owner、message non-consumption与AccRef cleanup不变。
   - Focused closure：至少覆盖 ordinary/stable action各自的 empty index、non-empty index及invalid index shape；每项必须在 consume前返回 error，callbacks/state完全相等，并证明没有 cancel_timer/demonitor side effect。随后重跑 supplied blocking matrix与 `async_t_SUITE`，不得出现新 signature。
   - 剩余风险：empty orphan index会破坏 absence/ownership不变量，并可能让后续 inspection、cleanup或key reuse面对已消费 action的残留metadata。
   - 是否可 merge：否。

2. **Finding 5 的 ordinary/stable duplicate timer 子项已闭合。**
   - 证据：`src/async_sf_cc.erl:40-65` 在创建 timer前执行完整 action preflight并调用 `timer_reference/3`；`:139-156` 拒绝 stable action携带ordinary sidecar；`:545-579` 分离ordinary sidecar与stable record的读取、写入和删除。ordinary/stable已有timer时均在 `send_after/3` 前返回 `duplicate_timer`，不会覆盖旧 reference。
   - 剩余风险：无新的 production signature；现有 `timer_reference/3` 的 stable-sidecar读取分支被前置preflight封闭，但不改变唯一 representation语义。
   - 是否可 merge：是，本子项不阻断。

3. **Finding 5 的 single demonitor owner 子项已闭合。**
   - 证据：`src/async_t.erl` 已删除 `handle_reference/3` 后的 `demonitor_ref/2` 调用及helper；bounded search在 `src/async_sf_cc.erl`、`src/async_t.erl` 中只剩 `src/async_sf_cc.erl:502-508` 的 `cleanup_request/1` 调用 `erlang:demonitor/2`。
   - 剩余风险：无；reply/timeout/DOWN的monitor cleanup现在由 `async_sf_cc`单一拥有。
   - 是否可 merge：是，本子项不阻断。

# Verdict:

changes_required

# Changes:

- Correction owner：`task-coding-2`。
- 只修正 `src/async_sf_cc.erl` 的 action-specific reverse-index preflight，明确拒绝任何 action-owned `?CHILDREN(Key)` entry，包括 shape-valid empty index；不得修改 tests、OpenSpec、plan、status 或 lanes。
- 不重开已闭合的 timer representation与single demonitor owner；新 immutable checkpoint只需提供 `b380f21..new` delta、上述empty/non-empty/invalid index focused evidence、完整 blocking self-validation和既有 `async_t_SUITE`结果。

# Caveats:

- 本轮没有重新运行 supplied validation matrix；命令结论采用用户提供的 independent evidence。唯一额外执行是针对 immutable candidate已编译 beam的只读 empty-index最小复现。
- `async_t_SUITE` 四项 AccRef legacy failures继续由既有 downstream owner `task-test-2`关闭；本 review没有扩展accepted signature。
- 本轮未修改 source、tests、OpenSpec、plan、status 或 lanes，未 stage、commit、merge或delegate；唯一写入是本 review artifact。
