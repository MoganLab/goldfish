# (liii pi ai tools definition) 模块函数索引

`(liii pi ai tools definition)` 定义工具定义 record `<pi-tool-definition>` 及其构造器、谓词与字段访问器。

使用时导入：

```scheme
(import (liii pi ai tools definition))
```

## 函数分类索引

- `pi-tool-definition` - 构造 `<pi-tool-definition>` 实例
- `pi-tool-definition?` - 判断是否为 `<pi-tool-definition>` 实例
- `pi-tool-definition-name` - 获取工具名称
- `pi-tool-definition-description` - 获取工具描述
- `pi-tool-definition-parameters` - 获取工具参数 schema
- `pi-tool-definition-execute` - 获取工具执行函数
- `pi-tool-definition-snippet` - 获取提示词片段
- `pi-tool-definition-guidelines` - 获取使用指南列表
