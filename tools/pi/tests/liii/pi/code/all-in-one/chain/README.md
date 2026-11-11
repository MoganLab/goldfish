# (liii pi code all-in-one chain) 模块函数分类索引

`(liii pi code all-in-one chain)` 实现粘性节点机制与链式调度：优先探活当前粘性节点（上一成功节点），若可用则直接复用；若粘性节点不可用，则按优先级顺序重新从链头开始探活。首个健康节点接收边界清洗后的会话并执行真实业务，全链失败返回 `all-nodes-exhausted`。

使用时导入：

```scheme
(import (liii pi code all-in-one chain))
```

## 查看函数文档

```bash
gf doc pi-chain-execute
gf doc pi-chain-node-label
gf doc pi-chain-reset-rewind-state!
gf doc pi-chain-reset-sticky-state!
gf doc pi-chain-clear-sticky-node!
gf doc pi-chain-current-sticky-index
```

## 函数分类索引

- `pi-chain-execute` - 粘性节点探活、链头优先回溯、边界清洗与真实请求执行
- `pi-chain-node-label` - 构造 `provider/model` 日志标签
- `pi-chain-reset-rewind-state!` - 清空当前粘性节点记忆与 REWIND 判定状态
- `pi-chain-reset-sticky-state!` - 清空当前粘性节点记忆（`pi-chain-reset-rewind-state!` 别名）
- `pi-chain-clear-sticky-node!` - 清除指定链的粘性节点记忆
- `pi-chain-current-sticky-index` - 查询指定链当前粘性节点的零基下标
