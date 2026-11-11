# (liii pi code all-in-one chain) 模块函数分类索引

`(liii pi code all-in-one chain)` 每轮从链头顺次调用预检，首个健康节点接收边界清洗后的会话并执行真实业务。主力节点恢复后自然回溯，不维护持久禁用或冷却状态；全链失败返回 `all-nodes-exhausted`。

使用时导入：

```scheme
(import (liii pi code all-in-one chain))
```

## 查看函数文档

```bash
gf doc pi-chain-execute
gf doc pi-chain-node-label
gf doc pi-chain-reset-rewind-state!
```

## 函数分类索引

- `pi-chain-execute` - 链头优先的预检、边界清洗、故障顺延与真实请求执行
- `pi-chain-node-label` - 构造 `provider/model` 日志标签
- `pi-chain-reset-rewind-state!` - 清空仅用于 REWIND 日志判定的成功节点记忆
