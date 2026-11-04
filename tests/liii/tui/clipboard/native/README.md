# (liii tui clipboard native) 模块函数索引

`(liii tui clipboard native)` 提供底层跨平台原生系统剪贴板工具调用支持，按操作系统调用系统剪贴板命令（Windows 的 clip，macOS 的 pbcopy，Linux 的 wl-copy/wl-paste/xclip/xsel）。

使用时导入：

```scheme
(import (liii tui clipboard native))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc native-clipboard-copy!
```

## 函数分类索引

### 原生剪贴板
- `native-clipboard-copy!` - 调用操作系统原生剪贴板工具写入文本
- `native-clipboard-text` - 在 Linux 上通过 wl-paste/xclip/xsel 读取外部剪贴板文本
