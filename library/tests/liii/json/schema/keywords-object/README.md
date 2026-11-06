# (liii json schema keywords-object) 模块函数分类索引

`(liii json schema keywords-object)` 实现 JSON Schema 的对象类关键字：
`properties`、`required`、`additionalProperties`、`propertyNames`、
`minProperties`、`maxProperties`、`dependentRequired`、`dependentSchemas`、
`unevaluatedProperties`、`patternProperties`。

JSON 对象统一使用键值对 alist 表示，`properties` 与 `additionalProperties`
通过 `descend` 下钻到子 Schema。

## 常见用法示例

```scheme
(import (liii json schema keywords-object))

;; 判断关键字归属
(object-keyword? "properties")   ; => 真值

;; 校验单个关键字
(validate-object #f "required" #("a") '(("b" . 1)) '(("required" . #("a")))
                 '() '() #f descend valid?)   ; => 错误列表
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/keywords-object "函数名"
```

## 函数分类索引

### 一、关键字归属
- `object-keyword?` - 判断关键字是否属于对象类关键字

### 二、校验
- `validate-object` - 校验对象类关键字并返回错误列表

## 错误处理约定

- 实例不是 JSON 对象时 `validate-object` 返回空列表，不产生错误
- `patternProperties` 依赖正则后端，当前按注解忽略，不匹配任何键
