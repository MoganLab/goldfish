# (liii tui editor state) 模块函数索引

`(liii tui editor state)` 定义编辑器内部核心状态数据结构 `<editor-data>`，管理底层缓冲区、输入历史、撤销栈、粘贴注册表、视口滚动偏移与外观配置。

使用时导入：

```scheme
(import (liii tui editor state))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc %make-editor-data
```

## 函数分类索引

### 内部数据结构
- `%make-editor-data` - 构造编辑器内部数据记录
- `%editor-data?` - 判断是否为编辑器内部数据记录
