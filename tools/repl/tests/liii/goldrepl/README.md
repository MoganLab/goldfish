# (liii goldrepl) 模块导览

`(liii goldrepl)` 是 Goldfish Scheme 交互式 REPL 的聚合库，汇总并重新导出各个子模块的功能，
同时提供多行输入缓冲区与 REPL 入口函数。

子模块划分：

- `(liii goldrepl style)` - 终端着色与语法高亮
- `(liii goldrepl complete)` - 符号补全
- `(liii goldrepl history)` - 历史记录
- `(liii goldrepl eval)` - 求值与括号配对
- `(liii goldrepl meta)` - 元命令
- `(liii goldrepl render)` - 界面渲染
- `(liii goldrepl loop)` - 主循环

使用时导入（load-path 需包含 `tools/repl`）：

```scheme
(import (liii goldrepl))
```

## 查看函数文档

```bash
# 查看模块导览（load-path 需包含 tools/repl）
gf doc liii/goldrepl

# 查看具体函数的文档
gf doc liii/goldrepl "main"
```

## 函数分类索引

### 输入缓冲区

- `%repl-buffer` - 创建多行输入缓冲区，初始内容为空行列表。
- `%repl-buffer-set!` - 替换缓冲区中的多行输入内容。

### 入口

- `main` - 启动 Goldfish Scheme 交互式 REPL，需要真实的终端环境。
