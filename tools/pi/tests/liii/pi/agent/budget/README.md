# (liii pi agent budget) 模块函数索引

`(liii pi agent budget)` 提供 Agent 轮数限制与执行时间预算的临界预警功能，仅在剩余预算接近耗尽时返回静态告警文本，避免逐轮变化的预算状态击穿供应商的前缀缓存。

使用时导入：

```scheme
(import (liii pi agent budget))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-budget-warning
```

## 函数分类索引

### 预算预警
- `pi-budget-warning` - 判断预算是否临界，仅临界时返回静态 <budget_warning> 告警块
