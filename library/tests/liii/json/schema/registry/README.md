# (liii json schema registry) 模块函数分类索引

`(liii json schema registry)` 实现 JSON Schema 资源登记表：
把 URI 与 Schema 的映射登记起来，供 `$ref` 解析使用。

登记表是不可变的，登记资源会返回新的登记表。

## 常见用法示例

```scheme
(import (liii json schema registry))

(define registry
  (json-schema-registry-add (make-json-schema-registry)
    "urn:integer"
    '(("type" . "integer")))
) ;define
(registry-resources registry)   ; => (("urn:integer" . (("type" . "integer"))))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/registry "函数名"
```

## 函数分类索引

### 一、创建
- `make-json-schema-registry` - 创建空的 JSON Schema 资源登记表

### 二、判断
- `json-schema-registry?` - 判断对象是否为 JSON Schema 资源登记表

### 三、登记
- `json-schema-registry-add` - 向资源登记表中登记一个资源
- `json-schema-registry-add-many` - 批量向资源登记表中登记资源

## 错误处理约定

- 登记表不是登记表或 URI 不是字符串时抛出类型错误
- URI 重复登记时抛出 `json-schema-options-error`
- 资源既不是对象也不是布尔值时抛出 `json-schema-schema-error`
