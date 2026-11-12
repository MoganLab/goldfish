# (liii pi code all-in-one thinking) 模块函数分类索引

`(liii pi code all-in-one thinking)` 负责把用户显式等级、节点默认等级与真实物理模型能力收敛成最终生效的 Thinking Level，并提供收敛诊断日志。

```scheme
(import (liii pi code all-in-one thinking))
```

## 函数分类索引

- `resolve-node-thinking-level` - 计算节点最终生效的 Thinking Level
- `pi-log-thinking-clamp!` - 显式等级被节点能力收敛时写 `[THINKING_CLAMP]` 日志
