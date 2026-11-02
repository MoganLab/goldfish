# (liii tui editor kill) 模块函数索引

`(liii tui editor kill)` 提供编辑缓冲区的整行剪切/删除、词删除与字符互换操作，对齐 Emacs/Readline 标准快捷键（Ctrl-K/Ctrl-U/Ctrl-W/Ctrl-T）。

使用时导入：

```scheme
(import (liii tui editor kill))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc buffer-kill-line-right!
```

## 函数分类索引

### 删除与剪切
- `buffer-kill-line-right!` - 删除到行尾（Ctrl-K）
- `buffer-kill-line-left!` - 删除到行首（Ctrl-U）
- `buffer-kill-word-left!` - 向左删除词（Ctrl-W）
- `buffer-transpose-chars!` - 交换相邻字符（Ctrl-T）
