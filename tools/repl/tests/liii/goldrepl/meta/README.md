# (liii goldrepl meta) 模块导览

`(liii goldrepl meta)` 提供 REPL 的元命令能力：列出可用的元命令、按子串搜索符号、
描述符号的类型与签名，并分发以逗号开头的元命令输入。

使用时导入（load-path 需包含 `tools/repl`）：

```scheme
(import (liii goldrepl meta))
```

## 查看函数文档

```bash
# 查看模块导览（load-path 需包含 tools/repl）
gf doc liii/goldrepl/meta

# 查看具体函数的文档
gf doc liii/goldrepl/meta "%repl-handle-meta"
```

## 函数分类索引

### 元命令表

- `*repl-meta-commands*` - 元命令表，每项形如 `(命令名 说明 是否精确匹配)`。
- `%repl-meta-help` - 返回元命令帮助信息的行列表。

### 符号检索

- `%repl-meta-apropos` - 按子串搜索符号缓存，返回描述行列表。
- `%repl-meta-describe` - 描述符号的类型、参数个数与文档，返回描述行列表。

### 元命令分发

- `%repl-handle-meta` - 分发以逗号开头的输入，返回 `(退出标志 . 输出行列表)`。
