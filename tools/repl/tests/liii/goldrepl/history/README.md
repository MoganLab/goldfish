# (liii goldrepl history) 模块导览

`(liii goldrepl history)` 提供 REPL 的历史记录能力：定位历史文件位置、对历史条目做转义与反转义、
解析历史文件内容，并把历史记录载入或保存到磁盘。

使用时导入（load-path 需包含 `tools/repl`）：

```scheme
(import (liii goldrepl history))
```

## 查看函数文档

```bash
# 查看模块导览（load-path 需包含 tools/repl）
gf doc liii/goldrepl/history

# 查看具体函数的文档
gf doc liii/goldrepl/history "%repl-load-history!"
```

## 函数分类索引

### 历史文件位置

- `%repl-history-dir` - 返回历史文件所在目录，遵循 `XDG_DATA_HOME`（Windows 为 `APPDATA`）约定。
- `%repl-history-file` - 返回历史文件的完整路径。

### 历史条目转义

- `%repl-escape-history-entry` - 把条目中的反斜杠与换行转义成单行文本。
- `%repl-unescape-history-entry` - 把单行文本反转义回原始条目。

### 历史文件读写

- `%repl-history-file-lines` - 解析历史文件，返回 `(条目数 . 条目列表)`。
- `%repl-load-history!` - 读取历史文件并载入历史对象，返回 `(条目数 . 条目列表)`。
- `%repl-save-history!` - 把历史条目写入历史文件。
