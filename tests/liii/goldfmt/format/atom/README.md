# (liii goldfmt format atom) 模块函数索引

`(liii goldfmt format atom)` 提供 goldfmt 格式化阶段的原子级辅助能力：原子值的文本格式化、注释与换行节点的识别、keyword 参数处理以及分隔符与缩进的计算。

使用时导入：

```scheme
(import (liii goldfmt format atom))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/format/atom "函数名"
```

## 函数分类索引

### 原子值格式化
- `format-atom-value` - 将原子值格式化为显示文本
- `spaces` - 生成 n 个空格组成的字符串
- `child-separator` - 计算父节点第 child-index 个子节点之前应输出的分隔符

### 注释与换行节点
- `comment-node?` - 判断 node 是否为注释节点
- `comment-content` - 读取注释节点的文本内容
- `format-comment-content` - 将注释内容格式化为 ;; 注释行
- `contains-comment?` - 判断 node 树中是否包含注释节点
- `newline-node?` - 判断 node 是否为换行节点
- `newline-count` - 读取换行节点中的换行数量
- `make-newlines` - 将换行标记数量转换为实际的换行符串

### quote 与 reader 前缀
- `quote-env?` - 判断 node 是否为 quote 节点
- `quote-env-content` - 读取 quote 节点中的内容
- `reader-prefix-env?` - 判断 node 是否为 quasiquote / unquote 节点

### keyword 参数
- `keyword-node?` - 判断 node 是否为 keyword atom
- `keyword-node-len` - 计算 keyword atom 显示文本的长度
- `keyword-block-prefix` - 计算首行可以内联摆放的连续 keyword 参数对数量与最大 key 长度
- `no-keyword-args-form?` - 判断某个 tag 是否为禁止 keyword 参数的表单

### 布局常量
- `fill-min-children` - 使用 fill 布局时至少保留的子节点数量
- `let-form?` - 判断某个 tag 是否为 let 类表单
