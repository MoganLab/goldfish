# (liii goldfmt scan literal) 模块函数索引

`(liii goldfmt scan literal)` 提供 reader 字面量的预处理能力：检测源码文本中是否包含 raw string 与字符字面量，并把这类字面量改写为普通 datum 形式，以便后续的 `read` 流程能够正确处理。

使用时导入：

```scheme
(import (liii goldfmt scan literal))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfmt/scan/literal "函数名"
```

## 函数分类索引

### 字面量改写
- `rewrite-reader-literals` - 将源码文本中的 reader 字面量改写为普通 datum 形式
- `needs-rewrite-reader-literals?` - 判断源码文本是否包含 reader 字面量
