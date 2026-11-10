# (liii goldfix tokenize) 模块函数索引

`(liii goldfix tokenize)` 提供括号修正所需的词法分析能力：把源码切分为带位置信息的 token，并按物理行组织这些 token。

使用时导入：

```scheme
(import (liii goldfix tokenize))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfix/tokenize "函数名"
```

## 函数分类索引

### 分词
- `tokenize` - 把源码切分为带 offset / line / column 的 token 列表

### 按行组织
- `tokenize-lines` - 按物理行组织 token，并记录每行的第一枚 code token

### token 判定
- `code-token?` - 判断 token 是否为 code token（排除注释）
- `line-start-close?` - 判断 token 是否为行首的右括号
