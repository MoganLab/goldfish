# (liii pi ai cost) 模块函数索引

`(liii pi ai cost)` 提供各 AI 模型 token 费用计算、缓存节省统计以及默认定价费率表管理。

使用时导入：

```scheme
(import (liii pi ai cost))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-calculate-cost
```

## 函数分类索引

### 费用计算
- `pi-calculate-cost` - 计算单次请求的各项 token 费用及总计
- `pi-calculate-cache-savings` - 计算缓存读取相比常规输入所节省的费用
- `pi-summarize-session-cost` - 统计会话总费用与总节省

### 费率管理
- `pi-model-cost` - 查询特定模型在指定服务商下的定价费率
- `PI_DEFAULT_MODEL_COSTS` - 内置服务商与模型的默认费率表
