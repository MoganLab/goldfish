# (liii pi review auto) 模块函数分类索引

`(liii pi review auto)` 提供工具调用自动审查注入功能，从 write/edit 工具调用中提取修改文件，并在下一轮对话前注入轻量代码自查要求。

使用时导入：

```scheme
(import (liii pi review auto))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-edited-files-from-tool-calls
gf doc pi-build-review-injection
```

## 函数分类索引

### 自动审查注入
- `pi-edited-files-from-tool-calls` - 从工具调用列表中提取由 write 或 edit 修改的文件路径与语言信息
- `pi-build-review-injection` - 构建自动审查注入文本块
