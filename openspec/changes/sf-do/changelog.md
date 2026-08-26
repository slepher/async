# Changelog

<!-- NEXT_CHANGE: 在此标记下方、分隔线上方追加后续变更，最新内容置顶。 -->

---

## 2026-08-24

- `sf_do` public source 收敛为 `sf_do(Comprehension)` 与
  `sf_do(BlockName, Comprehension)`；StepName 改为紧邻非最终 action 的 inline literal atom
  marker，不再使用额外 list/record 参数。
- standalone literal atom qualifier 只允许作为 StepName marker；duplicate、dangling、连续 marker、
  修饰 pure let/final action 或其他位置均为 source error。unnamed boundary 产生 warning，允许普通
  执行和 same-version resume，但 appup/appdown migration 必须失败。
- migration identity 只使用 Protocol、BlockPath 和 StepName；删除 derived identity、Action
  canonicalization、AsyncForm digest 及 AST/index fuzzy matching。
- Pass 1 使用 Astranaut return channel 累积 errors/warnings；有 error 时直接
  `astranaut_return:to_compiler/1` 且不运行 Pass 2，只有 warning 时继续 rewrite。Pass 2 只消费冻结
  plan，不重复 source validation。
- scope-aware capture 明确区分 `case`/`receive`/`try` outer equality reference 与
  `fun`/named-fun fresh binding，并要求共享 lexical binding classifier。
