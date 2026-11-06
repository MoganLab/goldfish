# (liii diff matcher) 模块函数分类索引

`(liii diff matcher)` 负责计算两个行向量之间的匹配块、差异操作码与 hunk 分组，是 `(liii diff)` 的算法内核。

## 查看函数文档

```bash
gf doc liii/diff/matcher
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/diff/matcher "get-opcodes"
```

## 函数分类索引

### 一、匹配块
- `get-matching-blocks` - 计算两个行向量之间的最长匹配块序列

### 二、操作码
- `get-opcodes` - 把匹配块序列转换为差异操作码序列

### 三、hunk 分组
- `get-grouped-opcodes` - 把差异操作码按上下文行数聚合为 hunk 分组
