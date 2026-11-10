# (liii json schema) 模块导览

`(liii json schema)` 是 JSON Schema 2020-12 校验库，支持对象、数组、字符串、
数字、布尔、null 六种实例类型，覆盖组合关键字、引用、动态引用、未求值位置
与格式断言等 2020-12 语义。

使用时导入：

```scheme
(import (liii json schema))
```

## 查看函数文档

```bash
gf doc liii/json/schema
gf doc "json-schema-valid?"
gf doc "make-json-schema-validator"
gf doc "json-schema-error-kind"
```

子模块文档：

```bash
gf doc liii/json/schema/error
gf doc liii/json/schema/data
gf doc liii/json/schema/reference
```

## 函数分类索引

### 一、校验入口

便捷入口，每次调用重新构造校验器：

| 函数 | 说明 |
|---|---|
| `json-schema-valid?` | 判断实例是否满足 Schema |
| `json-schema-validate` | 校验实例，失败时抛出异常 |
| `json-schema-errors` | 收集全部错误 |
| `json-schema-check-schema` | 只检查 Schema 本身是否合法 |

### 二、可复用校验器

创建一次校验器后可反复使用，避免重复编译 Schema：

| 函数 | 说明 |
|---|---|
| `make-json-schema-validator` | 创建校验器 |
| `json-schema-validator?` | 判断是否为校验器 |
| `json-schema-validator-valid?` | 用校验器判断实例是否合法 |
| `json-schema-validator-errors` | 用校验器收集全部错误 |
| `json-schema-validator-validate` | 用校验器校验实例，失败时抛出异常 |

### 三、错误对象访问器

错误对象为 `<json-schema-error>` 记录：

| 函数 | 说明 |
|---|---|
| `json-schema-error?` | 判断是否为错误对象 |
| `json-schema-error-kind` | 错误类别 |
| `json-schema-error-message` | 错误消息 |
| `json-schema-error-keyword` | 触发错误的关键字 |
| `json-schema-error-keyword-value` | 关键字的值 |
| `json-schema-error-instance` | 触发位置的实例 |
| `json-schema-error-schema` | 触发位置的 Schema |
| `json-schema-error-path` | 实例路径 |
| `json-schema-error-schema-path` | Schema 路径 |
| `json-schema-error-context` | 组合关键字的子错误 |
| `json-schema-error-cause` | 底层原因 |
| `json-schema-error-absolute-path` | 从根开始的实例路径 |
| `json-schema-error-absolute-schema-path` | 从根开始的 Schema 路径 |
| `json-schema-error-json-path` | JSONPath 风格路径 |

### 四、外部资源注册表

引用外部 Schema 时通过注册表管理资源：

| 函数 | 说明 |
|---|---|
| `make-json-schema-registry` | 创建空注册表 |
| `json-schema-registry?` | 判断是否为注册表 |
| `json-schema-registry-add` | 注册一个资源 |
| `json-schema-registry-add-many` | 批量注册资源 |

## 常见用法示例

### 校验对象实例

```scheme
(define person-schema
  '(("$schema" . "https://json-schema.org/draft/2020-12/schema")
    ("type" . "object")
    ("properties"
      ("name" ("type" . "string"))
      ("age" ("type" . "integer") ("minimum" . 0)))
    ("required" . #("name" "age"))
    ("additionalProperties" . #f)))

(json-schema-valid? '(("name" . "Alice") ("age" . 18)) person-schema) ; => #t
(json-schema-valid? '(("name" . "Alice") ("age" . -1)) person-schema) ; => #f
```

### 收集全部错误

```scheme
(map json-schema-error-json-path (json-schema-errors '(("age" . -1)) person-schema))
; => ("$.age")
```

### 复用校验器

```scheme
(define validator (make-json-schema-validator person-schema '((check-schema? . #t))))
(json-schema-validator-valid? validator '(("name" . "A") ("age" . 1))) ; => #t
```

### 注册外部资源

```scheme
(define registry
  (json-schema-registry-add (make-json-schema-registry)
    "https://example.com/geo.json"
    '(("$id" . "https://example.com/geo.json")
      ("type" . "object")
      ("properties" ("lat" ("type" . "number"))))))

(json-schema-valid? '(("lat" . 12.5))
  '(("$ref" . "https://example.com/geo.json"))
  `((registry . ,registry) (check-schema? . #f))) ; => #t
```
