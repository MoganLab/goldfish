(import (liii check) (liii json schema) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-valid?
;; 判断实例是否满足 JSON Schema。
;;
;; 语法
;; ----
;; (json-schema-valid? instance schema . options)
;;
;; 参数
;; ----
;; instance : any?
;; 待校验的 JSON 数据。
;;
;; schema : any?
;; JSON Schema，可以是对象或布尔值。
;;
;; options : alist?
;; 可选选项表，详见 make-json-schema-validator。
;;
;; 返回值
;; ----
;; boolean
;; 满足 Schema 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 便捷入口默认先按 2020-12 元 Schema 检查 Schema 本身。
;; 需要复用校验器或关闭元 Schema 检查时请使用 make-json-schema-validator。
;;
;; 示例
;; ----
;; (json-schema-valid? '(("name" . "Alice")) '(("type" . "object"))) => #t
;;
;; 错误处理
;; ----
;; json-schema-options-error 当选项非法时。
;; json-schema-schema-error 当 Schema 非法时。
;; json-schema-unknown-dialect 当 $schema 不是 2020-12 时。
;; json-schema-unresolvable 当引用无法解析时。

(define schema
  '(("type" . "object")
    ("properties" ("name" ("type" . "string")))
    ("required" . #("name")))
) ;define

;; 对象实例
(check-true (json-schema-valid? '(("name" . "Liii")) schema))
(check-false (json-schema-valid? '(()) schema))
(check-false (json-schema-valid? '(("name" . 1)) schema))

;; 布尔 Schema：true 接受一切，false 拒绝一切
(check-true (json-schema-valid? '(("anything" . #(1))) #t))
(check-true (json-schema-valid? 'null 'true))
(check-false (json-schema-valid? 'null #f))
(check-false (json-schema-valid? 1 'false))

;; 解析后的 JSON 文本可以直接参与校验

(define parsed-schema
  (string->json "{\"type\":\"array\",\"items\":{\"type\":\"integer\"}}")
) ;define
(check-true (json-schema-valid? (string->json "[1,2,3]") parsed-schema))
(check-false (json-schema-valid? (string->json "[1,\"x\"]") parsed-schema))

;; 未知关键字被忽略，未知方言被拒绝
(check-true (json-schema-valid? 'null '(("x-unknown" . #f)
                                        ("title" . "annotation")))
) ;check-true
(for-each
  (lambda (uri)
    (check-catch 'json-schema-unknown-dialect
      (json-schema-valid? 1 `(("$schema" . ,uri)))
    ) ;check-catch
  ) ;lambda
  '("https://json-schema.org/draft/2019-09/schema"
    "http://json-schema.org/draft-07/schema#"
    "http://json-schema.org/draft-06/schema#"
    "http://json-schema.org/draft-04/schema#"
    "http://json-schema.org/draft-03/schema#" "urn:unknown")
) ;for-each

;; 非法选项
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((unknown-option . #t)))
) ;check-catch
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((max-errors . -1)))
) ;check-catch
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((max-errors . 0)))
) ;check-catch
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((draft . draft2020-12)))
) ;check-catch
;; gf >= 18.11.47: list? 严格化为 proper-list?，点对列表 options 被拒绝。
(check-catch 'json-schema-options-error
  (json-schema-valid? 1 #t '((max-errors . 1) . bad))
) ;check-catch

;; 类型断言：每个 JSON 类型都有对应的关键字取值。

(check-true (json-schema-valid? 'null '(("type" . "null"))))
(check-true (json-schema-valid? #t '(("type" . "boolean"))))
(check-true (json-schema-valid? '(()) '(("type" . "object"))))
(check-true (json-schema-valid? #() '(("type" . "array"))))
(check-true (json-schema-valid? "x" '(("type" . "string"))))
(check-true (json-schema-valid? 1.5 '(("type" . "number"))))
(check-true (json-schema-valid? 1 '(("type" . "integer"))))
(check-true (json-schema-valid? 1.0 '(("type" . "integer"))))
(check-false (json-schema-valid? 1.5 '(("type" . "integer"))))
(check-false (json-schema-valid? #t '(("type" . "number"))))
(check-false (json-schema-valid? #f '(("type" . "integer"))))
(check-false (json-schema-valid? 'null '(("type" . "string"))))
(check-true (json-schema-valid? "1" '(("type" . #("integer" "string")))))
(check-true (json-schema-valid? 1 '(("type" . #("integer" "string")))))
(check-false (json-schema-valid? #t '(("type" . #("integer" "string")))))
(check-catch 'json-schema-schema-error
  (json-schema-valid? 1 '(("type" . "unknown")))
) ;check-catch

(check-report)
