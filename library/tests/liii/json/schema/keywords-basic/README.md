# (liii json schema keywords-basic) 模块函数分类索引

`(liii json schema keywords-basic)` 实现 JSON Schema 的基础断言类关键字：
`type`、`const`、`enum`、`minimum`、`maximum`、`exclusiveMinimum`、
`exclusiveMaximum`、`multipleOf`、`minLength`、`maxLength`。

这些关键字只约束实例本身，不涉及对象成员或数组元素的下钻，
因此校验失败时只产生一条错误记录。

## 常见用法示例

```scheme
(import (liii json schema keywords-basic))

;; 判断关键字归属
(basic-keyword? "type")   ; => 真值

;; 校验单个关键字
(validate-basic "minimum" 2 1 '(("minimum" . 2)) '() '())  ; => 错误列表

;; 数值倍数判定
(multiple-of? 9 0.3)   ; => #t
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/keywords-basic "函数名"
```

## 函数分类索引

### 一、关键字归属
- `basic-keyword?` - 判断关键字是否属于基础断言类关键字

### 二、校验
- `validate-basic` - 校验基础断言类关键字并返回错误列表

### 三、数值判定
- `multiple-of?` - 判断实例是否为目标除数的整数倍

## 错误处理约定

- `multiple-of?` 的除数为 0 或非数值时抛出 `json-schema-schema-error`
- `validate-basic` 遇到未知 `type` 名称时抛出 `json-schema-schema-error`
- 字符串长度按 Unicode 码点计数，不按 UTF-8 字节数
