# (liii goldfmt tokenize) 模块函数索引

`(liii goldfmt tokenize)` 提供 Scheme 源码文本的词法切分能力：把原始文本切分为 code、comment、newline 三类 token，并支持将 token 还原为可读文本以及字符串内容的转义。

使用时导入：

```scheme
(import (liii goldfmt tokenize))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/tokenize "函数名"
```

## 函数分类索引

### 词法切分
- `tokenize` - 将源代码文本切分为 token 序列

### token 还原与转义
- `tokens->string` - 将 token 序列还原为可读文本
- `escape-string-content` - 对字符串内容进行转义处理
