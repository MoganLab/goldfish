# (liii goldrepl render) 模块导览

`(liii goldrepl render)` 提供 REPL 的界面渲染能力：生成横幅与提示符、把输入行渲染成带高亮的文本行、
刷新屏幕，并把求值输出、错误输出与求值结果整理成输出行。

使用时导入（load-path 需包含 `tools/repl`）：

```scheme
(import (liii goldrepl render))
```

## 查看函数文档

```bash
# 查看模块导览（load-path 需包含 tools/repl）
gf doc liii/goldrepl/render

# 查看具体函数的文档
gf doc liii/goldrepl/render "%repl-banner-lines"
```

## 函数分类索引

### 横幅与提示符

- `%repl-s7-version-text` - 返回 S7 Scheme 版本说明文本。
- `%repl-mode-desc-line` - 返回当前模式的说明行。
- `%repl-banner-lines` - 返回 REPL 启动横幅的行列表。
- `%repl-prompt` - 返回首行提示符或续行提示符。

### 输入渲染

- `%repl-render-input-lines` - 把编辑器内容渲染成带提示符与语法高亮的行列表。
- `%repl-refresh!` - 把滚动区、输入行与附加行渲染到屏幕。

### 输出展示

- `%repl-show-out` - 把求值产生的标准输出整理成输出行。
- `%repl-show-err` - 把求值产生的错误输出整理成输出行。
- `%repl-show-val` - 把求值结果注册为会话变量并整理成输出行。

### 编辑帮助

- `%repl-editor-help-lines` - 返回编辑命令帮助信息的行列表。
