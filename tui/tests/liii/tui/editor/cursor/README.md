# (liii tui editor cursor) 模块函数索引

`(liii tui editor cursor)` 提供编辑缓冲区光标定位与移动操作，覆盖上下左右步进（支持多行折叠）、行首行尾跳转、按单词跳跃与单词边界计算。

使用时导入：

```scheme
(import (liii tui editor cursor))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc buffer-move-left!
```

## 函数分类索引

### 基础光标移动
- `buffer-move-left!` - 光标左移（行首时折回上一行末）
- `buffer-move-right!` - 光标右移（行末时折至下一行首）
- `buffer-move-up!` - 光标上移一行
- `buffer-move-down!` - 光标下移一行
- `buffer-move-home!` - 移动到行首
- `buffer-move-end!` - 移动到行尾

### 单词级跳跃
- `buffer-move-word-left!` - 向左跳单词
- `buffer-move-word-right!` - 向右跳单词
- `%word-left-boundary` - 计算单词左边界索引
