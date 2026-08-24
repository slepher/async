# sf-migrate

## Why

`sf-do` 已自动继承 identity 未变的 pending frame：一、二参数形式按 BlockName、
AsyncIndex 和 canonical AsyncForm 派生；三参数形式按 BlockName + StepName 命名。目标
manifest 仍有相同 AsyncId/MFA 时，旧 reply 直接进入目标版本同名 continuation function，
不需要 migration declaration 或 API。

`sf-migrate` 解决两类显式 code-change：一类是无法自动识别的 async boundary，例如在前面
插入/删除/重排 async 导致 AsyncIndex 变化，async action form 改写，named/anonymous block
identity 改变、derived/named mode 切换或 StepName 改名；另一类是 identity 未变但 Env 需要显式
转换，例如 capture 变量改名或同名变量的 value 含义变化。
runtime 不得在这些情况下使用 index 接近、AST/capture 相似度或 bind name 猜测
target；未显式迁移时默认失败。

这类历史 boundary 继承必须显式声明：用户提供完整 historical do block、将旧版本
锚定到该 block，并分别列出允许 upgrade 与 downgrade 的 async mapping。`old`
永远表示 historical/较旧版本，
`new` 永远表示当前编译/较新版本；upgrade 执行 `old -> new`，downgrade 执行
`new -> old`。

## What Changes

- 新增 `sf_migrate(OldDoBlock, Spec)`，以及 named historical block 使用的
  `sf_migrate(StepNames, OldDoBlock, Spec)` module-level declaration。
- migration 用一个双向 `transition` 引用两个 block endpoint；endpoint 可以是命名 atom，
  也可以是匿名 `{Function, Arity, N}`。upgrade 与 downgrade 使用同一模型和 API。
- 遵循 OTP dynamic callback 的 advanced update 时序：upgrade 在较新代码加载后执行
  `code_change(Vsn, ...)`；downgrade 在旧代码加载前由较新代码执行
  `code_change({down, TargetVsn}, ...)`。
- 每个历史 block 以完整 `[async_m || ...]` derived 形式，或通过
  `sf_migrate(StepNames, [async_m || ...], Spec)` named 形式保存在当前源码中，并具有显式 historical
  block key。StepNames 与 `sf_do` 一样支持 record 名或 atom literal list。
- 支持 `v1 => block_1, v2 => block_1, v3 => block_2, v4 => current` 一类 version
  anchors；upgrade source 未锚定时保持 unhandled，downgrade target 未锚定且存在 pending
  current frame 时阻止 code rollback。
- `steps` 的 `directions` 分别是 upgrade/downgrade allowlist；用户可只开放一个方向，
  正向 Env migrator 不得被当作反向 migrator 使用。
- 显式 step mapping 通常连接 source/target AsyncId 不同的边界。AsyncId 相同的 frame 默认由
  `sf-do` 自动继承；但 mapping 若为至少一个 enabled direction 显式配置 `migrate_env`，则允许作为
  Env-only migration，只转换 capture-name map，保持 AsyncId/MFA 不变。没有 Env migrator 的
  same-identity mapping 仍是 redundant。
- 支持 named identity 的 `{step, StepName}`、1-based source index selector、唯一
  name/content selector、正向 Offset 和可选
  capture Env migration MFA。source index 统一计数 `bind|then|let`；`bind` 与 `then` 可作为
  migration target，纯计算 `let` 只参与定位和计数，不可迁移。
- declaration 或 step mapping 可分别声明 `up_state` 与 `down_state` source capture；
  `state => Name` 是两者同为 Name 的 shorthand。declaration-level 值是所有 step 的默认值，
  step 只能继承或重复同方向值，不得覆盖为不同 capture name。Env migrator context 使用
  `source_state` 表示迁移源 frame 捕获的 State，并保留 `current_state` 为传入
  `migrate_sf_callbacks/4` 的第四参数原值。
- Env 原则上不包含 anonymous/local fun。若 SourceEnv 已包含，应用应通过 directional Env
  migrator 在 target code 中重新生成或改写为 external MFA/data descriptor；升级不会自动清除或
  更新 State 中的旧 fun，source body 未变化也不是兼容依据。该 regeneration support 是可选后续
  能力，不作为当前 sf-migrate initiative 的强制验收。
- 扩展 `async_sf_transform`，在分析当前 `sf_do` 后分析历史 block 并交叉校验。
- 在 `code_change/3` 显式调用 migration API；runtime 只替换共享 callbacks field 中
  `#sf_callback.frame`。
- downgrade 必须先把业务 State 转成目标旧版 layout，再使用目标 callbacks offset 迁移
  current frames；全部 pending stable entries 可处理后，OTP 才能加载旧 module。
- 匿名与命名 endpoint 间迁移只要求用户填写 `{Function, Arity, N}`，不得要求 generated
  atom、AsyncId 或 hash。
- 保留 request key、monitor、timer、accumulator reference 和 completion target；不重放
  workflow 或重新发送 request。
- terminal mismatch callback 只允许 abort，不允许用自定义 replacement frame 绕过
  编译期 step allowlist。
- migration result 显式区分 `source_block/source_async` 与 `target_block/target_async`；API
  infrastructure error 使用结构化 stage/reason context，并返回未修改的入口 TargetState。
- 当前功能尚未发布，directional manifest 直接替换旧草案并继续使用
  `format => 1, protocol => 1`；不实现旧草案 manifest 的兼容 reader 或升级路径。
- CaptureAbi 固定为排序后的 capture-name atom list，不做额外 hash。CaptureAbi 不同但该方向
  未配置 Env migrator 时不得编译失败。新版无法访问真正运行中的
  旧 frame Env；compiler 只记录 metadata，runtime 根据实际 source frame 进入 mismatch。
- 保持 `async_m:migrate_sf_callbacks/4` 为薄 facade，migration orchestration 位于独立
  `async_sf_migration` 模块，并复用已有 State/callback、stable-term 和 completion 能力。
- legacy `#callback{}`、普通 do 和未声明迁移的 stable block 不受影响。

## Capabilities

### New Capabilities

- `sf-migrate`: 完整历史 block、版本 anchor、显式 boundary/Env-only mapping、capture migration 和
  terminal mismatch policy。

## Dependencies

- 依赖 `sf-do` 的 `sf_frame()`、`#sf_callback{}`、shared callbacks router、current
  manifest 和 `async_sf_transform` orchestrator。
- 必须在 `sf-do` 完成并验证后实现。

## Impact

- `async_sf_transform` 增加 historical analyzer、selector resolver 和 migration manifest。
- `async_m` 增加 code-change migration facade。
- 使用方需要保存历史 block 声明，并在相关 OTP process 的 `code_change/3` 显式调用。
- 使用方文档需要覆盖 declaration/options、upgrade/downgrade、selector、State context、
  result/error shape 和故障排查。
- 不新增 callback store、frame runner 或 `async_sf_t`。
