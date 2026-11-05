# (liii goldrepl eval) 模块导览

`(liii goldrepl eval)` 提供 REPL 的求值能力：统计输入文本中未闭合的括号、在根环境中求值并捕获
标准输出与错误输出，同时把求值结果注册为带序号的会话变量。

使用时导入（load-path 需包含 `tools/repl`）：

```scheme
(import (liii goldrepl eval))
```

## 查看函数文档

```bash
# 查看模块导览（load-path 需包含 tools/repl）
gf doc liii/goldrepl/eval

# 查看具体函数的文档
gf doc liii/goldrepl/eval "%repl-eval"
```

## 函数分类索引

### 括号配对

- `%repl-missing-parens` - 统计文本中未闭合的括号数量，字符串与注释内容不参与统计。

### 求值

- `%repl-eval` - 在根环境中求值，并返回 `(标准输出 错误输出 是否成功 求值结果)`。
- `*repl-eval-error*` - 求值失败时使用的哨兵值。

### 会话值

- `%repl-register-value!` - 把求值结果注册为 `$序号` 形式的会话变量，并返回该变量名。
- `*repl-value-count*` - 已经注册的会话值数量。
