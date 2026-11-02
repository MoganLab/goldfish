# (liii tui clipboard) 模块函数索引

`(liii tui clipboard)` 提供终端剪贴板操作支持，支持使用 OSC 52 转义序列以及各操作系统原生剪贴板工具实现剪贴板内容写入。

使用时导入：

```scheme
(import (liii tui clipboard))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc clipboard-copy!
```

## 函数分类索引

### 剪贴板复制
- `clipboard-copy!` - 将文本复制到终端（OSC 52）及系统原生剪贴板
- `native-clipboard-copy!` - 重导出自 `(liii tui clipboard native)`，调用系统原生剪贴板命令写入文本

### OSC 52 转义序列
- `osc52-sequence` - 将字符串编码为 OSC 52 剪贴板写入 ANSI 转义序列
- `osc52-copy-sequence` - `osc52-sequence` 的别名
