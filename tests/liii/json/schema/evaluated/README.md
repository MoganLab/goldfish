# (liii json schema evaluated) 模块函数分类索引

`(liii json schema evaluated)` 计算 JSON Schema 的求值注解：
对象实例中哪些属性已经被断言覆盖，数组实例中哪些元素已经被断言覆盖。

`unevaluatedProperties` 与 `unevaluatedItems` 依赖这些注解判断
哪些成员仍未被求值。

## 常见用法示例

```scheme
(import (liii json schema evaluated))

;; 对象中已求值的属性
(evaluated-properties #f '(("a" . 1) ("b" . 2))
                      '(("properties" . (("a" . (("type" . "integer"))))))
                      #f valid?)   ; => ("a")

;; 数组中已求值的下标
(evaluated-items #f #(1 2 3)
                '(("items" . #((("type" . "integer")))))
                #f valid?)   ; => (0 1 2)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/evaluated "函数名"
```

## 函数分类索引

### 一、对象注解
- `evaluated-properties` - 收集对象实例中已被求值的属性名

### 二、数组注解
- `evaluated-items` - 收集数组实例中已被求值的元素下标

## 错误处理约定

- 实例类型不匹配或 Schema 不是 JSON 对象时返回空列表
- `patternProperties` 依赖正则后端，当前按注解忽略，匹配的属性不算已求值
