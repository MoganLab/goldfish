# (liii goldfix cli) 模块函数索引

`(liii goldfix cli)` 提供 gf fix 命令行的解析能力：帮助文档输出、参数解析器构造、位置参数提取以及文件后缀名解析。

使用时导入：

```scheme
(import (liii goldfix cli))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfix/cli "函数名"
```

## 函数分类索引

### 命令行帮助
- `display-help` - 输出 gf fix 命令的用法与帮助信息

### 参数解析
- `make-fix-arg-parser` - 创建 gf fix 的参数解析器（help / dry-run / extension / changed-since）
- `first-positional` - 取出参数解析器中的第一个位置参数

### 文件后缀名
- `normalize-extension` - 规范化单个文件后缀名（补齐前导点）
- `parse-extensions` - 解析逗号分隔的多个文件后缀名
