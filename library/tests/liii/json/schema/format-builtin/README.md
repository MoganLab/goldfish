# (liii json schema format-builtin) 模块函数分类索引

`(liii json schema format-builtin)` 提供 JSON Schema `format` 关键字的内置
格式检查器表，把格式名映射到具体的检查过程。

表中的格式覆盖 IP、邮箱、主机名、日期时间与 URI / JSON Pointer 系列。

## 常见用法示例

```scheme
(import (liii json schema format-builtin))

(cdr (assq 'date json-schema-format-builtins))   ; => 日期检查过程
((cdr (assq 'date json-schema-format-builtins)) "2020-01-01")   ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/format-builtin "函数名"
```

## 函数分类索引

### 一、内置检查器表
- `json-schema-format-builtins` - 内置格式检查器表

## 错误处理约定

- 表是常量，调用方不应修改
- 表中每一项都是「格式名 . 检查过程」，检查过程接受字符串返回布尔值
