# (liii goldfmt format reader) 模块函数索引

`(liii goldfmt format reader)` 提供 reader 风格的 datum 文本化能力：把归一化后的 datum 按 Scheme reader 的书写习惯格式化为文本，支持内联、缩进换行以及 quote 家族前缀写法。

使用时导入：

```scheme
(import (liii goldfmt format reader))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/format/reader "函数名"
```

## 函数分类索引

### datum 文本化
- `format-reader-datum` - 将 datum 按 reader 风格格式化为文本（零缩进）
- `format-reader-datum-at` - 将 datum 按 reader 风格格式化为文本，从指定缩进列开始计量
- `format-reader-datum-inline` - 将 datum 强制内联格式化（不换行）
