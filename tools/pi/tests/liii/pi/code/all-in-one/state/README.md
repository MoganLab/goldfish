# (liii pi code all-in-one state) 模块函数分类索引

`(liii pi code all-in-one state)` 持有 all-in-one 的进程级调度状态：链 ID 到粘性节点下标的映射、节点身份键、节点最近故障时间戳以及冷却窗口判断。它不含任何调度算法、网络或 CLI 依赖，调度算法在 `(liii pi code all-in-one scheduler)`。

使用时导入：

```scheme
(import (liii pi code all-in-one state))
```

## 状态归属

| 状态 | 结构 | 语义 |
|---|---|---|
| 粘性下标 | `((chain-id . index) ...)` | 每条链记住最近一次成功执行的真实节点，正常路径优先复用 |
| 节点故障时间戳 | `((node-key . timestamp-ms) ...)` | 每个节点只保留最近一次真实失败或探活失败的时间 |

两份表都是进程级、跨请求保留的单表；同一 key 只保留最新一条记录。节点身份键为
`provider/model:thinkingLevel` 三元组，日志标签只用 `provider/model`，两者都是 state 内部实现，保持私有。

## 时间与冷却

时间注入支持毫秒数或无参过程，缺省用系统单调时钟（`current-monotonic-millis`）。
冷却窗口 `cooldown-ms <= 0` 视为禁用冷却；故障时间戳在未来（时钟回拨）时按未冷却处理。

冷却期到期后重新参与调度是冷却机制的既定目的，因此 state 不提供“本轮失败永久排除”能力，也不改变冷却默认值。

## 查看函数文档

```bash
gf doc pi-chain-current-sticky-index
gf doc pi-chain-record-sticky-node!
gf doc pi-chain-clear-sticky-node!
gf doc pi-chain-record-node-failure!
gf doc pi-chain-clear-node-failure!
gf doc pi-chain-node-last-failure
gf doc pi-chain-node-in-cooldown?
gf doc pi-chain-reset-state!
gf doc pi-chain-reset-cooldown-state!
```

## 函数分类索引

### 粘性节点状态

- `pi-chain-current-sticky-index` - 查询指定链当前粘性节点的零基下标
- `pi-chain-record-sticky-node!` - 记录指定链的粘性节点下标
- `pi-chain-clear-sticky-node!` - 清除指定链的粘性节点记忆

### 节点冷却状态

- `pi-chain-record-node-failure!` - 记录或刷新节点的故障时间戳
- `pi-chain-clear-node-failure!` - 清除指定节点的故障记录
- `pi-chain-node-last-failure` - 查询节点的最近一次故障时间戳
- `pi-chain-node-in-cooldown?` - 判定节点当前是否处于故障冷却期内

### 状态重置

- `pi-chain-reset-state!` - 同时清空粘性记忆与节点冷却，供场景测试回到初始状态
- `pi-chain-reset-cooldown-state!` - 只清空节点冷却记录（压缩窗口等只关心冷却的调用方使用）

## 导出边界

历史上曾把「只清粘性」和「只清 REWIND 记忆」拆成两个别名
（`pi-chain-reset-sticky-state!`、`pi-chain-reset-rewind-state!`），两者实际都同时
清空粘性与冷却，命名与行为不一致，已收敛为单一的 `pi-chain-reset-state!`。
