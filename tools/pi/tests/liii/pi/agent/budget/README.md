# (liii pi agent budget) 模块函数索引

`(liii pi agent budget)` 提供 Agent 轮数限制与执行时间预算状态的格式化功能，支持在单轮提示词中注入当前轮数、剩余时间及超限告警信息。

使用时导入：

```scheme
(import (liii pi agent budget))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-format-turn-budget-status
```

## 函数分类索引

### 预算状态
- `pi-format-turn-budget-status` - 格式化轮数与时间预算状态提示文本
