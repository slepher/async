# sf-migrate

## Why

`sf-do` 已经解决无需业务转换的跨版本恢复：pending frame 在 reply 到达时，用自身
`module` 查询当前 `'$async_sf_manifest'/0`；只要 named identity
`{Protocol, Block, StepName}` 仍存在，且当前 Step 所需 Env keys 都在 saved Env 中，就直接进入
当前 Step。module Vsn 不参与 boundary identity。

`sf-migrate` 只补充 automatic path 无法表达的 **old → current** 转换：Block/StepName 改名，
Outcome shape 改变，Env capture 被删除、改名，或同名 capture 的业务含义改变。它不扫描或改写
State 中的 callback，而是在该 callback 真正收到 Outcome 时，由当前 FrameOwner 选择并执行
显式 bridge。

## What Changes

- migration declaration 与 `sf_do` 位于同一个 FrameOwner module；StateHolder module 不拥有
  frame migration。
- migration-enabled FrameOwner 必须声明显式 module `-vsn`。frame 继续保存创建它的
  `module` 与 literal module Vsn；app Vsn 不进入 frame identity。
- 用户实现 `sf_do_code_change/2`，按 `{OldModuleVsn, OldBlock}` 使用普通 Erlang clause、guard
  或 `case` 路由到一个精确的 `sf_migrate/3`：

  ```erlang
  sf_do_code_change(OldVsn, OldBlock) ->
      case {OldVsn, OldBlock} of
          {v1, order_flow} ->
              sf_migrate(v1,
                  [async_m || request_v1,
                      Reply <- promise_v1(), finish_v1(Reply)],
                  #{from => order_flow, to => order_flow,
                      steps => #{request_v1 => #{to => request}}});
          {v2, order_flow} ->
              sf_migrate(v2,
                  [async_m || request_v2,
                      Reply <- promise_v2(), finish_v2(Reply)],
                  #{from => order_flow, to => order_flow,
                      steps => #{request_v2 => #{to => request}}});
          _ ->
              unhandled
      end.
  ```

- `sf_migrate(OldModuleVsn, OldBlock, Spec)` 把一个 exact historical module Vsn 与完整 old
  comprehension 放在一起；`Spec.from`保存其old BlockName。多个 historical Vsn 使用多个调用，
  不建立 version graph 或自动多跳。
- Spec 只列出 explicit old StepName → current StepName mapping，以及可选的用户手写
  `convert/2` external MFA。
- parse transform 从完整 old block 与 current block 得到 source/target Outcome shape、Env capture
  shape和generated Step MFA；它不推断业务值转换。
- runtime 不替换 `#sf_callback.frame`。它生成并调用 old-interface bridge：bridge 接收原
  `(Outcome, Env)`，可调用用户 converter 得到 current `(Outcome, Env)`，再转发到 current Step。
- Vsn 不同本身不是 failure。没有 explicit mapping 时仍先遵循 sf-do 的 named identity + Env
  subset automatic compatibility。
- downgrade 不执行 reverse converter，也不在 app down 时扫描或 fail callbacks。降级后的
  current FrameOwner manifest 若可直接接管 frame，就正常执行；否则 callback 在实际触发时返回
  structured incompatible failure。Outcome 在 down path 原样传给目标 Step。
- 不新增 `async_m:migrate_sf_callbacks/4`、State callbacks offset、release_handler hook、version
  registry、down candidate、dependency attribute 或 upgrade-order enforcement。
- ordinary `do`、ordinary `#callback{}`、unnamed `sf_do` frame 和 sf-cc lifecycle 保持不变。

## Capabilities

### New Capabilities

- `sf-migrate`: exact historical module Vsn + complete old block declaration、显式 old/current Step
  mapping、reply-time Outcome/Env bridge，以及 structured incompatibility failure。

## Dependencies

- 复用 `sf-do` 的 frame、named identity、generated Step MFA、current manifest 与 lazy resolver。
- 复用 `sf-cc` 的完整 `#sf_callback{frame,acc_ref,parent_ref,timer_ref}` 和 direct
  `parent_ref` failure forwarding。
- `sf-do`、`sf-cc` 的已实现口径优先；本 change 不复制或改写其协议。

## Impact

- `async_sf_transform` 增加 `sf_do_code_change/2`、historical comprehension 和 `sf_migrate/3` 分析，
  并生成 one-way bridge。
- `async_sf_runtime` 在现有 manifest resolution 中增加 explicit plan fallback/override。
- FrameOwner 使用方需要维护其愿意支持的完整 historical blocks 和 exact module Vsns。
- StateHolder、`code_change/3`、appup/relup 和 callback store layout 不需要为 sf-migrate 改动。

## Deferred

- app/module dependency order、循环引用 warning 与 upgrade-order建议可由未来独立 Rebar 插件完成；
  本 change 不生成 `-sf_migration_dependencies`，也不阻止任意 release 顺序。
- async runtime 自身跨不兼容 `#sf_callback{}` encoding 的升级属于 async release compatibility，
  不属于应用 frame migration。
