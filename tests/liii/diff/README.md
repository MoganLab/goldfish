# (liii diff) 模块函数分类索引

`(liii diff)` 提供纯内存的文本行差异比对与标准统一差异（Unified Diff）生成功能，对标 Python 官方标准库 difflib，支持多 Hunk 自动聚合与序列相似度计算。

## 查看函数文档

```bash
gf doc liii/diff
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/diff "unified-diff"
```

## 函数分类索引

### 一、统一差异生成
- `unified-diff` - 生成行列表的标准统一差异补丁行列表
- `unified-diff-string` - 生成多行字符串的标准统一差异补丁字符串

### 二、序列相似度
- `diff-ratio` - 计算两个序列的相似度得分（0.0 ~ 1.0）

## 子模块

- `(liii diff hunk)` - hunk 头与差异行格式化：`format-range-unified`、`format-group`、`format-all-groups`、`make-diff-header`
- `(liii diff matcher)` - 匹配块与操作码计算：`get-matching-blocks`、`get-opcodes`、`get-grouped-opcodes`
