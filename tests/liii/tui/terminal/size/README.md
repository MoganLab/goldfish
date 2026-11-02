# (liii tui terminal size) 模块函数索引

`(liii tui terminal size)` 提供终端尺寸探测能力，优先读取环境变量 `COLUMNS` 与 `LINES`，并在取值非法时安全回退到默认的 80x24。

使用时导入：

```scheme
(import (liii tui terminal size))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc terminal-detect-size
```

## 函数分类索引

### 尺寸探测
- `terminal-detect-size` - 探测终端窗口尺寸（列数与行数）
