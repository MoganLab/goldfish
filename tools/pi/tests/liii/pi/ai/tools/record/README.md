# (liii pi ai tools record) 模块函数索引

`(liii pi ai tools record)` 提供工具定义、工具执行结果与文本截断结果的包装构造器与 alist 转换接口，支持结构体与关联列表双向操作。

使用时导入：

```scheme
(import (liii pi ai tools record))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-tool-definition
```

## 函数分类索引

### 工具定义
- `pi-tool-definition` - 构造工具定义 record
- `pi-tool-definition->alist` - 将工具定义转换为 alist

### 工具执行结果
- `pi-tool-result` - 构造工具执行结果 record
- `pi-tool-result-isError` - 获取是否为错误结果（别名）
- `pi-tool-result->alist` - 将工具执行结果转换为 alist

### 文本截断结果
- `pi-trunc-result` - 构造文本截断结果 record
- `pi-trunc-result-truncated?` - 是否发生截断（谓词别名）
- `pi-trunc-result-first-line-truncated` - 首行是否超限截断
- `pi-trunc-result-last-line-truncated` - 末行是否部分截断
- `pi-trunc-result->alist` - 将截断结果转换为 alist
