# (liii pi code all-in-one chain) 模块函数分类索引

`(liii pi code all-in-one chain)` 实现粘性节点机制、链式调度与故障冷却（Circuit Breaker）：优先探活当前粘性节点（上一成功节点），若可用则直接复用；若粘性节点不可用，触发重新调度时对高优先级节点应用冷却时间窗口（复用配置中的 `retryIntervalMs`，缺省 60 秒），冷却期内节点直接跳过以避免连续超时，超出冷却期节点优先探活自愈回溯。首个健康节点接收边界清洗后的会话并执行真实业务，全链失败返回 `all-nodes-exhausted`。

使用时导入：

```scheme
(import (liii pi code all-in-one chain))
```

## 查看函数文档

```bash
gf doc pi-chain-execute
gf doc pi-chain-node-label
gf doc pi-chain-node-key
gf doc pi-chain-reset-rewind-state!
gf doc pi-chain-reset-sticky-state!
gf doc pi-chain-clear-sticky-node!
gf doc pi-chain-current-sticky-index
gf doc pi-chain-record-node-failure!
gf doc pi-chain-clear-node-failure!
gf doc pi-chain-node-last-failure
gf doc pi-chain-node-in-cooldown?
gf doc pi-chain-reset-cooldown-state!
```

## 函数分类索引

- `pi-chain-execute` - 粘性节点探活、冷却期防雪崩、链头优先自愈回溯、边界清洗与真实请求执行
- `pi-chain-node-label` - 构造 `provider/model` 日志标签
- `pi-chain-node-key` - 生成节点唯一标识键字符串（`provider/model:thinkingLevel`）
- `pi-chain-reset-rewind-state!` - 清空当前粘性节点记忆、REWIND 判定状态与节点冷却故障记录
- `pi-chain-reset-sticky-state!` - 清空当前粘性节点记忆与节点冷却故障记录（`pi-chain-reset-rewind-state!` 别名）
- `pi-chain-clear-sticky-node!` - 清除指定链的粘性节点记忆
- `pi-chain-current-sticky-index` - 查询指定链当前粘性节点的零基下标
- `pi-chain-record-node-failure!` - 记录或刷新节点的故障时间戳
- `pi-chain-clear-node-failure!` - 清除指定节点的故障记录
- `pi-chain-node-last-failure` - 查询节点的最近一次故障时间戳
- `pi-chain-node-in-cooldown?` - 判定节点当前是否处于故障冷却期内
- `pi-chain-reset-cooldown-state!` - 清空所有节点的故障时间戳与冷却状态
