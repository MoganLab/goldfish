# (liii pi ai event) 模块函数索引

`(liii pi ai event)` 提供 Agent 事件的 JSON 序列化、统一事件构造分发及支持 record 字段访问的增强 `assoc`。

使用时导入：

```scheme
(import (liii pi ai event))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-serialize-event
```

## 函数分类索引

### 事件序列化与构造
- `pi-serialize-event` - 将事件序列化为单行 JSON 字符串
- `pi-build-json-event` - 根据类型动态构造事件对象
- `assoc` - 兼容事件 record 与标准 alist 的键查找
