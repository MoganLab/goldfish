;; (liii json schema) 模块介绍与用法示例
;;
;; (liii json schema) 是 JSON Schema 2020-12 校验库，支持对象、数组、
;; 字符串、数字、布尔、null 六种实例类型，覆盖组合关键字、引用、动态引用、
;; 未求值位置与格式断言等 2020-12 语义。

(import (liii json schema))

;; ==== 模块概述 ====
;;
;; 公开入口分为两类：
;;
;; - 便捷入口：每次调用重新构造校验器
;;   json-schema-valid?    判断实例是否满足 Schema
;;   json-schema-validate  校验实例，失败时抛出异常
;;   json-schema-errors    收集全部错误
;;   json-schema-check-schema  只检查 Schema 本身是否合法
;;
;; - 可复用校验器
;;   make-json-schema-validator      创建校验器
;;   json-schema-validator-valid?    用校验器判断实例是否合法
;;   json-schema-validator-errors    用校验器收集全部错误
;;   json-schema-validator-validate  用校验器校验实例，失败时抛出异常
;;
;; 错误对象为 <json-schema-error> 记录，访问器见 json-schema-error-kind。
;; 外部资源通过 make-json-schema-registry 与 json-schema-registry-add 管理。

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema
;;
;; 查看具体函数文档：
;;   gf doc "json-schema-valid?"
;;   gf doc "make-json-schema-validator"
;;   gf doc "json-schema-error-kind"
;;
;; 查看子模块文档：
;;   gf doc liii/json/schema/error
;;   gf doc liii/json/schema/data
;;   gf doc liii/json/schema/reference

;; ==== 函数分类索引 ====
;;
;; 一、校验入口
;;   json-schema-valid?           - 判断实例是否满足 Schema
;;   json-schema-validate         - 校验实例，失败时抛出异常
;;   json-schema-errors           - 收集全部错误
;;   json-schema-check-schema     - 只检查 Schema 本身
;;
;; 二、可复用校验器
;;   make-json-schema-validator   - 创建校验器
;;   json-schema-validator?       - 判断是否为校验器
;;   json-schema-validator-valid? - 用校验器判断实例是否合法
;;   json-schema-validator-errors - 用校验器收集全部错误
;;   json-schema-validator-validate - 用校验器校验实例
;;
;; 三、错误对象访问器
;;   json-schema-error?              - 判断是否为错误对象
;;   json-schema-error-kind          - 错误类别
;;   json-schema-error-message       - 错误消息
;;   json-schema-error-keyword       - 触发错误的关键字
;;   json-schema-error-keyword-value - 关键字的值
;;   json-schema-error-instance      - 触发位置的实例
;;   json-schema-error-schema        - 触发位置的 Schema
;;   json-schema-error-path          - 实例路径
;;   json-schema-error-schema-path   - Schema 路径
;;   json-schema-error-context       - 组合关键字的子错误
;;   json-schema-error-cause         - 底层原因
;;   json-schema-error-absolute-path - 从根开始的实例路径
;;   json-schema-error-absolute-schema-path - 从根开始的 Schema 路径
;;   json-schema-error-json-path     - JSONPath 风格路径
;;
;; 四、外部资源注册表
;;   make-json-schema-registry - 创建空注册表
;;   json-schema-registry?     - 判断是否为注册表
;;   json-schema-registry-add  - 注册一个资源
;;   json-schema-registry-add-many - 批量注册资源

;; ==== 常见用法示例 ====

;; 示例1：校验一个对象实例

(define person-schema
  '(("$schema" . "https://json-schema.org/draft/2020-12/schema")
    ("type" . "object")
    ("properties"
      ("name" ("type" . "string"))
      ("age" ("type" . "integer") ("minimum" . 0)))
    ("required" . #("name" "age"))
    ("additionalProperties" . #f))
) ;define

(json-schema-valid? '(("name" . "Alice") ("age" . 18)) person-schema) ; => #t
(json-schema-valid? '(("name" . "Alice") ("age" . -1)) person-schema) ; => #f

;; 示例2：收集全部错误

(map json-schema-error-json-path (json-schema-errors '(("age" . -1)) person-schema))
; => ("$.age")

;; 示例3：校验失败时抛出异常

(json-schema-validate '(("name" . "Alice") ("age" . 18)) person-schema)
; => (("name" . "Alice") ("age" . 18))

;; 示例4：复用校验器

(define validator (make-json-schema-validator person-schema '((check-schema? . #t))))

(json-schema-validator-valid? validator '(("name" . "A") ("age" . 1))) ; => #t
(json-schema-validator-errors validator '(("name" . "A") ("age" . -1)))

;; 示例5：注册外部资源

(define registry
  (json-schema-registry-add (make-json-schema-registry)
    "https://example.com/geo.json"
    '(("$id" . "https://example.com/geo.json")
      ("type" . "object")
      ("properties" ("lat" ("type" . "number")))))
) ;define

(json-schema-valid? '(("lat" . 12.5))
  '(("$ref" . "https://example.com/geo.json"))
  `((registry . ,registry) (check-schema? . #f)))
; => #t
