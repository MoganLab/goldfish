# (liii pi agent cost) 模块函数索引

`(liii pi agent cost)` 提供 Agent 消息的 Token 开销计算与会话总成本统计功能，根据模型费率表自动为 Assistant 消息附加 `cost` 字段，并汇总会话所有轮次的 Token 与费用消耗。

使用时导入：

```scheme
(import (liii pi agent cost))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-agent-attach-cost
```

## 函数分类索引

### 成本计算与附加
- `pi-agent-attach-cost` - 根据配置的模型费率计算并附加单次响应的消耗成本
- `pi-agent-session-cost-summary` - 汇总会话所有 Assistant 消息的 Token 与美元总开销
