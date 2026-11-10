# (liii goldfmt format) 模块函数索引

`(liii goldfmt format)` 提供 goldfmt 的格式化入口：把 Scheme datum 或 node 树排版为符合项目风格的多行文本，并返回带位置信息的新 node 树。

使用时导入：

```scheme
(import (liii goldfmt format))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/format "函数名"
```

## 函数分类索引

### datum 格式化
- `format-datum` - 格式化一个 Scheme datum，并返回格式化文本
- `format-datum+node` - 格式化一个 Scheme datum，并同时返回带位置信息的新 node

### node 树格式化
- `format-node` - 从指定列开始格式化一个 node
- `format-nodes` - 格式化一组 node 树，并返回拼接后的文本
- `format-string` - 格式化 source string 中的所有 top-level datum

### 内联判定
- `format-inline` - 将 node 转成单行候选字符串
- `can-inline?` - 判断一个 node 是否可以按单行形式输出
