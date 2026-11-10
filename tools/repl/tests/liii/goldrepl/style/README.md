# (liii goldrepl style) 模块导览

`(liii goldrepl style)` 提供 REPL 的终端着色能力：维护 ANSI 样式表与关键字表，
并提供按行语法高亮的能力。

使用时导入（load-path 需包含 `tools/repl`）：

```scheme
(import (liii goldrepl style))
```

## 查看函数文档

```bash
# 查看模块导览（load-path 需包含 tools/repl）
gf doc liii/goldrepl/style

# 查看具体函数的文档
gf doc liii/goldrepl/style "%repl-highlight-line"
```

## 函数分类索引

### 样式表

- `*repl-styles*` - 样式名到 ANSI 控制序列的映射表。
- `%repl-style` - 按样式名取出对应的 ANSI 控制序列。
- `%repl-styled` - 用指定样式包裹文本，并追加复位控制序列。
- `%repl-gray` - 用灰色样式包裹文本。

### 语法高亮

- `*repl-keywords*` - 需要按关键字着色的 Scheme 关键字列表。
- `%repl-highlight-line` - 对一行 Scheme 文本做语法高亮，返回带 ANSI 控制序列的文本。
