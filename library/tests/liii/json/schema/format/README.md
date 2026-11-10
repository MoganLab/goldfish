# (liii json schema format) 模块函数分类索引

`(liii json schema format)` 实现 JSON Schema `format` 关键字的断言逻辑：
按 `formats` 选项决定哪些格式参与校验，
并按 `format-checkers` 选项查找自定义检查器。

默认情况下 `format` 只作为注解，不参与断言。

## 常见用法示例

```scheme
(import (liii json schema format))

;; formats 为 #t 时断言所有已知格式
(validate-format validator "email" "2962" schema '() '())

;; 自定义检查器优先于内置检查器
(json-schema-builtin-format-checkers)   ; => 内置检查器表
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/format "函数名"
```

## 函数分类索引

### 一、校验
- `validate-format` - 校验 format 关键字并返回错误列表

### 二、辅助
- `json-schema-format-checker?` - 判断对象是否为格式检查器
- `format-checker-applies?` - 判断某个格式是否参与断言
- `json-schema-builtin-format-checkers` - 返回内置格式检查器表

## 错误处理约定

- 实例不是字符串时不参与格式检查
- 未知格式名只作为注解，不产生错误
- `format-checkers` 中的检查器不是过程时抛出 `json-schema-options-error`
