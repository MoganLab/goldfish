# (liii pi code all-in-one scheduler) 模块函数分类索引

`(liii pi code all-in-one scheduler)` 实现链式调度算法：正常路径（首次调度与粘性复用）不探活，直接选用粘性节点或链上首个未冷却节点执行真实业务；仅在当前节点真实请求失败、需要切换节点时才对候选节点按优先级顺序探活，并对高优先级节点应用冷却时间窗口（复用配置中的 `retryIntervalMs`，缺省 60 秒），冷却期内节点直接跳过以避免连续超时，超出冷却期节点优先探活自愈回溯。首个选定节点接收边界清洗后的会话并执行真实业务，全链不可用返回 `all-nodes-exhausted`。

粘性下标、节点身份与故障时间戳都归 `(liii pi code all-in-one state)` 所有；本模块不保存跨调用状态，只调用 state 的公开接口。

使用时导入：

```scheme
(import (liii pi code all-in-one scheduler))
```

## 调度算法

给定按优先级排列的候选节点链 `[N_0, N_1, ..., N_{k-1}]`、冷却窗口 `cooldown`（`retryIntervalMs`，默认 60s）及当前粘性节点记忆：

1. **正常请求路径（不探活）**
   - 若存在粘性节点：直接选中该粘性节点发起真实请求，成功后保持粘性。
   - 若无粘性节点（首次请求或粘性已清除）：从链头起顺次检查，直接选中首个不在冷却期内的节点发起真实请求，并将其记为粘性节点。
   - 若所有节点均处于冷却期内：清除粘性、记录 `[EXHAUSTED]` 日志并返回 `'all-nodes-exhausted`。

2. **故障切换路径（仅此时探活）**
   当上述选中的节点在真实请求中失败时，由调用方记录其故障时间、清除粘性并跳过该故障节点，随后按优先级从链头开始扫描：
   - 若当前节点就是刚失败的节点：跳过；
   - 若当前节点仍在 60 秒冷却期内：跳过（不探活）；
   - 否则对该节点发起轻量探活：
     - **探活成功**：清除该节点的故障记录，更新粘性为该节点（若其优先级高于切换前节点则记录 `[REWIND]`，否则记录 `[SWITCH]`），对其发起真实请求并结束；
     - **探活失败**：记录或刷新该节点的故障时间，继续扫描下一节点。
   - 若所有候选节点均被跳过或探活失败：清除粘性、记录 `[EXHAUSTED]` 日志并返回 `'all-nodes-exhausted`。

冷却期到期后节点重新参与探活是冷却机制的既定目的，即使发生在同一轮也允许；调度不维护“本轮失败节点永久排除集合”。

## 场景示例

假设某链按优先级配置了三个节点：`[kimi-coding/k2.5, zai-coding-cn/glm-5, deepseek/deepseek-chat]`，冷却窗口 `60s`：

1. **首次请求（走正常路径，不探活）**
   - 无粘性节点且全链未冷却，直接选中链头 `kimi-coding/k2.5` 发起真实请求；成功后将其记为粘性节点。

2. **后续请求（走正常路径，不探活）**
   - 命中粘性节点 `kimi-coding/k2.5`，直接复用该节点发起真实请求。

3. **故障切换与冷却跳过（走切换路径，探活）**
   - 如果 `kimi-coding/k2.5` 真实请求失败：记录其进入 60 秒冷却期并触发切换，跳过 `kimi-coding/k2.5`，探活 `zai-coding-cn/glm-5` 成功，记录 `[SWITCH]` 并将粘性更新为 `zai-coding-cn/glm-5`。
   - 如果在冷却期内（例如 9 秒后）`zai-coding-cn/glm-5` 也失败：触发切换时，`kimi-coding/k2.5` 仍在 60 秒冷却期内会被直接跳过（不探活），同时跳过刚失败的 `zai-coding-cn/glm-5`，直接探活 `deepseek/deepseek-chat` 并切换过去。

4. **冷却期满重新探活（`[REWIND]`）**
   - 如果在 `kimi-coding/k2.5` 冷却期满后（例如又过了 60 秒），`deepseek/deepseek-chat` 失败：触发切换时，从链头扫描发现 `kimi-coding/k2.5` 已过冷却期，优先对其探活；
   - 若探活成功，则清除其故障记录、记录 `[REWIND]` 并将粘性切回高优先级的 `kimi-coding/k2.5`；若仍失败，则重新计时 60 秒冷却期并顺延探活下一未冷却节点。

## 全链结算

`pi-chain-settle-exhausted!` 是唯一的结算入口：写一条 `[EXHAUSTED]` 日志、清除该链粘性并返回 `'all-nodes-exhausted`。调度内部无候选可用，以及调用方用尽本轮真实请求预算（例如单节点链真实失败、末节点真实失败），都调用它，因此两条路径的日志、粘性与返回值完全一致。

## 查看函数文档

```bash
gf doc pi-chain-execute
gf doc pi-chain-settle-exhausted!
```

## 函数分类索引

### 调度

- `pi-chain-execute` - 仅切换节点时探活、冷却期防雪崩、链头优先重新探活、边界清洗与真实请求执行

### 全链结算

- `pi-chain-settle-exhausted!` - 写 EXHAUSTED 日志、清除链粘性并返回 exhausted 哨兵

## 导出边界

节点身份键（`provider/model:thinkingLevel`）与日志标签（`provider/model`）是调度内部实现，
保持私有；它们的行为由 `pi-chain-execute` 与 `pi-chain-settle-exhausted!` 的 SWITCH / REWIND / EXHAUSTED 日志断言覆盖。

粘性下标与冷却状态的读写接口在 `(liii pi code all-in-one state)`，由本模块与
`(liii pi code all-in-one request)`、`(liii pi code core compaction flow)` 共同使用。
