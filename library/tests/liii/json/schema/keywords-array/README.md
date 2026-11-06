# (liii json schema keywords-array) 模块函数分类索引

`(liii json schema keywords-array)` 实现 JSON Schema 的数组类关键字：
`prefixItems`、`items`、`minItems`、`maxItems`、`uniqueItems`、
`contains`、`unevaluatedItems`。

数组实例统一使用向量表示，`prefixItems` 与 `items` 通过 `descend`
下钻到子 Schema，`contains` 通过 `valid?` 统计匹配元素个数。

## 常见用法示例

```scheme
(import (liii json schema keywords-array))

;; 判断关键字归属
(array-keyword? "items")   ; => 真值

;; 校验单个关键字
(validate-array #f "minItems" 2 #(1) '(("minItems" . 2)) '() '() #f
                descend valid?)   ; => 错误列表
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/keywords-array "函数名"
```

## 函数分类索引

### 一、关键字归属
- `array-keyword?` - 判断关键字是否属于数组类关键字

### 二、校验
- `validate-array` - 校验数组类关键字并返回错误列表

## 错误处理约定

- 实例不是数组时 `validate-array` 返回空列表，不产生错误
- `uniqueItems` 按 JSON 语义比较元素，`1` 与 `1.0` 视为重复
