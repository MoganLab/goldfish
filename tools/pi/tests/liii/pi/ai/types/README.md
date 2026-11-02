# (liii pi ai types) 模块函数索引

`(liii pi ai types)` 聚合重导出了 AI 层的事件系统（`(liii pi ai event)`）与消息系统（`(liii pi ai message)`）的类型定义与访问器，并提供了消息和用量统计的快捷构造工具函数。

使用时导入：

```scheme
(import (liii pi ai types))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-make-user-message
```

## 函数分类索引

### 消息快捷构造
- `pi-make-user-message` - 快捷构造用户消息
- `pi-make-assistant-message` - 快捷构造助手消息
- `pi-make-tool-result-message` - 快捷构造工具结果消息

### 用量统计构造
- `pi-make-usage` - 构造 token 用量统计关联列表
