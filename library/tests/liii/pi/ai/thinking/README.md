# (liii pi ai thinking) 模块函数索引

`(liii pi ai thinking)` 提供思考级别（thinking level）的定义、模型能力判断、级别收敛以及生效级别优先级解析。

使用时导入：

```scheme
(import (liii pi ai thinking))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-resolve-thinking-level
```

## 函数分类索引

### 思考级别定义
- `pi-thinking-levels` - 思考级别全集列表
- `pi-default-thinking-level` - 默认思考级别
- `pi-thinking-level?` - 判断是否为合法的思考级别

### 模型能力判断
- `pi-json-true?` - 判断 JSON 布尔真值
- `pi-model-supports-reasoning?` - 判断模型是否支持推理
- `pi-model-thinking-levels` - 获取模型支持的思考级别集合

### 级别收敛与解析
- `pi-clamp-thinking-level` - 收敛思考级别至可用级别集合
- `pi-resolve-thinking-level` - 按优先级解析最终生效的思考级别
