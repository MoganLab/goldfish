(import (liii check) (liii json schema) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-validate
;; 校验实例并在通过时返回实例本身。
;;
;; 语法
;; ----
;; (json-schema-validate instance schema . options)
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
;; any?
;; 校验通过时原样返回 instance。
;;
;; 注意
;; ----
;; 与 json-schema-valid? 相同，但校验失败时抛出异常而不是返回 #f。
;;
;; 示例
;; ----
;; (json-schema-validate '(("name" . "Alice")) '(("type" . "object")))
;; => '(("name" . "Alice"))
;;
;; 错误处理
;; ----
;; json-schema-validation-error 当实例不满足 Schema 时。
;; json-schema-options-error 当选项非法时。
;; json-schema-schema-error 当 Schema 非法时。

(define schema
  '(("type" . "object")
    ("properties" ("name" ("type" . "string")))
    ("required" . #("name")))
) ;define

(check (json-schema-validate '(("name" . "Liii")) schema)
  =>
  '(("name" . "Liii"))
) ;check

(check-catch 'json-schema-validation-error (json-schema-validate '(()) schema))
(check-catch 'json-schema-validation-error (json-schema-validate 1 #f))
(check-catch 'json-schema-schema-error (json-schema-validate 1 '()))

(check-report)
