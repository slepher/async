# Test Task Review

Status: changes_required

Task: task-test-1

Reviewed commit: `bc28febe142000025abaf59c71c8b41ab6cffe99`

Previous reviewed commit: none

## Evidence

- 报告将 test-owned 修改路径精确限定为 `test/async_sf_SUITE.erl` 与 `test/async_t_SUITE.erl`，并明确未修改 `src/`、`include/`、`rebar.config`、OpenSpec、workflow/status/contracts。
- baseline focused reruns 精确记录 9 个 deferred failures：`async_sf_SUITE` 5 个、`async_t_SUITE` 4 个；final focused reruns 分别为 10/10 与 25/25，且报告逐项列出原失败 case/位置，因此这 9 个 deferred failures 已由最终 focused evidence 关闭。
- final full evidence 为 `rebar3 compile` exit 0、`rebar3 eunit` 12/12、`rebar3 ct` 51/51、`rebar3 xref` exit 0、`git diff --check` exit 0。
- 报告同时明确：真实 old-three-tuple State fixture、`async_sf_cc_SUITE`、独立 macro suite，以及 R3–R8、R10–R19 的直接 observable coverage 均未建立。

## Findings

1. 必需的 test-owned observable coverage 未完成。commit/report evidence 是报告的 `Findings and limits` 与 `Closure statement`：`async_histories_SUITE` 没有真实 old-three-tuple State fixture，`test/async_sf_cc_SUITE.erl` 和独立 macro suite 不存在，R3–R8、R10–R19 没有直接覆盖；报告也明确写明 task-test-1 observable completion 未建立。已供应的合同依据将这些列为必需项，而不是环境检查或仅供参考的后续工作。测试 owner 应在 task-test-1 获授权的 test fixture/suite seams 中补齐真实旧状态 fixture、CC suite、独立 macro suite，并为 R3–R8、R10–R19 增加直接、可观察、非重复的 assertions；保留当前九个 deferred case 的闭合行为。focused validation 必须覆盖新增 suite/fixture 与原 `async_sf_SUITE`、`async_t_SUITE`，随后 `rebar3 ct` 全量通过；observable closure 是新报告逐项映射并证明上述必需 coverage，而非仅引用间接既有测试。

## Verdict

changes_required

九个 deferred failures 的精确闭合可以接受，但它只关闭了报告中的 bounded fixture/expectation 子集；缺少的 fixture、suite 与 R3–R8/R10–R19 direct coverage 是 test-owned omissions，因此当前报告不能通过。这里不需要新的产品行为决定，故不属于 `context_blocked`。

## Changes

Correction owner: test

仅修正测试、fixture 或 harness，并提交 `task-test-1-report-2.md` 供下一轮 review；不得在本 correction 中修改产品代码或把产品问题作为修正指令。

## Routing

- Task: task-test-1.md
- Report: task-test-1-report-1.md
- Reviewed checkpoint: bc28febe142000025abaf59c71c8b41ab6cffe99
- Correction owner: test
- Test correction: add owned old-three-tuple/runtime/macro coverage and direct R3-R19 assertions
- Affected coding tasks: none
- Verdict: changes_required

## Caveats

- `rebar3 dialyzer` 的 140 个 warnings 位于 product/dependency `src` boundaries（报告点名 `src/async_sf_cc.erl:182,187`、`src/async_t.erl:439,922,1139`），不属于本轮 test correction；它们仍是未通过/未归因的验证结果，不能声称 passed。
- `npx openspec validate sf-cc --strict` 因 OpenSpec CLI 不可用而未验证；不能声称 schema passed。
- OTP 21–29 images/matrix 不可用，兼容性矩阵未验证；不能声称 passed。
- R1 仅有既有间接 source/diagnostic coverage，R2/R9 由四个 ordinary cleanup cases 覆盖；这不替代缺失的 R3–R8、R10–R19 direct coverage。
