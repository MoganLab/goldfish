# (liii goldfmt stem) 模块函数索引

`(liii goldfmt stem)` 提供 stem 模式的格式化入口：在 stem 模式下格式化源码文本或 node 树，使得 quote 与 quasiquote 模板片段不会被折叠。

使用时导入：

```scheme
(import (liii goldfmt stem))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/stem "函数名"
```

## 函数分类索引

### stem 模式格式化
- `format-stem-string` - 在 stem 模式下格式化一段源码文本
- `stem-format-nodes` - 在 stem 模式下格式化一组 node 树
