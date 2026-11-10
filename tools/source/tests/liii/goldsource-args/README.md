# (liii goldsource-args) 模块函数索引

`(liii goldsource-args)` 提供 `gf source` 子命令的命令行参数与库查询解析能力。

使用时导入：

```scheme
(import (liii goldsource-args))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldsource-args "函数名"
```

## 函数分类索引

### 库查询判定与解析
- `library-query?` - 判断给定值是否为库查询字符串（包含 "/"）
- `parse-library-query` - 解析形如 "组织/库名" 的查询字符串为 (组织 . 库名) 点对

### 命令行参数解析
- `parse-source-args` - 解析 `gf source` 的命令行参数列表
