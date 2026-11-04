# (liii goldfmt format walk) 模块函数索引

`(liii goldfmt format walk)` 提供 goldfmt 排版的核心游走过程：把 node 树写入 writer，并返回记录了缩进与行号定位信息的新 node 树。

使用时导入：

```scheme
(import (liii goldfmt format walk))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/format/walk "函数名"
```

## 函数分类索引

### 节点游走
- `walk!` - 将节点树写入 writer 并返回带定位信息的新树
