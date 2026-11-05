# (liii goldrepl loop) 模块导览

`(liii goldrepl loop)` 提供 REPL 的主循环能力：处理求值与补全交互、提交代码与元命令、
预加载 REPL 模式、排版补全菜单，并在退出时清理终端与保存历史。

使用时导入：

```scheme
(import (liii goldrepl loop))
```

## 查看函数文档

使用以下命令查看模块导览与函数文档：

```bash
bin/gf doc liii/goldrepl/loop
bin/gf doc liii/goldrepl/loop %repl-submit-code!
```

## 函数分类索引

### 求值与补全

- `%repl-eval-and-show!` - 求值代码并把输出、错误与结果输出到界面。
- `%repl-apply-completion!` - 用补全候选替换光标处的当前词并移动光标。
- `%repl-try-complete-first!` - 首次触发补全：单候选直接补全，多候选补全公共前缀。
- `%repl-try-complete-cycle!` - 循环切换补全候选。
- `%repl-format-completion-menu` - 排版补全菜单，最多显示 9 个候选。

### 提交输入

- `%repl-submit-meta!` - 提交以逗号开头的元命令，返回 `(退出标志 . 历史条目)`。
- `%repl-submit-code!` - 提交代码，括号未闭合时继续累积多行输入。

### 模式与清理

- `%repl-preload-mode!` - 按 REPL 模式预加载额外的模块。
- `%repl-cleanup!` - 退出 REPL 时保存历史并恢复终端状态。
- `%repl-mode` - 从命令行参数中解析 REPL 模式，缺省为 `liii`。
- `%repl-atom?` - 判断文本是否是原子表达式（不含括号）。
