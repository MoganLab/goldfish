# (liii json schema keywords) 模块函数分类索引

`(liii json schema keywords)` 是关键字校验的分派层：
按关键字类别把校验交给 `keywords-basic`、`keywords-object`、
`keywords-array` 与 `keywords-combo`，
并处理 `$ref`、`$dynamicRef` 与 `format`。

本模块同时维护元校验根资源的全局状态，
供 `check-schema` 过程把 `$dynamicRef` 解析到元 Schema。

## 常见用法示例

```scheme
(import (liii json schema keywords))

;; 分派单个关键字
(validate-keyword validator "type" "integer" 1 schema '() '() root descend valid?)

;; 在指定的元校验根资源下执行
(with-meta-validation-root meta-root (lambda () ...))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/keywords "函数名"
```

## 函数分类索引

### 一、关键字分派
- `validate-keyword` - 按关键字类别分派校验，并返回错误列表

### 二、元校验根
- `meta-validation-root` - 读取当前的元校验根资源
- `set-meta-validation-root!` - 设置当前的元校验根资源
- `with-meta-validation-root` - 在指定的元校验根资源下调用过程

## 错误处理约定

- 未知关键字直接忽略，不产生错误
- 元校验根资源是模块级全局状态，异常中断后应复位
- `with-meta-validation-root` 在过程抛出异常时也会恢复上一层设置
