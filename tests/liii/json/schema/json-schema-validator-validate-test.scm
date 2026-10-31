(import (liii check) (liii json schema) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-validator-validate
;; 用校验器校验实例，校验失败时抛出异常。
;;
;; 语法
;; ----
;; (json-schema-validator-validate validator instance)
;;
;; 参数
;; ----
;; validator : validator
;; 由 make-json-schema-validator 创建的校验器。
;;
;; instance : any?
;; 待校验的 JSON 数据。
;;
;; 返回值
;; ----
;; any?
;; 校验通过时原样返回 instance。
;;
;; 注意
;; ----
;; 与 json-schema-validator-valid? 相同，但校验失败时抛出异常而不是返回 #f。
;;
;; 示例
;; ----
;; (json-schema-validator-validate validator 1) => 1
;;
;; 错误处理
;; ----
;; json-schema-validation-error 当实例不满足 Schema 时。
;; type-error 当 validator 不是校验器时。

(define validator
  (make-json-schema-validator
    '(("type" . "object")
      ("properties" ("name" ("type" . "string")))
      ("required" . #("name")))
    '((check-schema? . #t)))
) ;define

(check (json-schema-validator-validate validator '(("name" . "Liii")))
  =>
  '(("name" . "Liii"))
) ;check

(check-catch 'json-schema-validation-error
  (json-schema-validator-validate validator '(("name" . 1)))
) ;check-catch
(check-catch 'json-schema-validation-error
  (json-schema-validator-validate validator '())
) ;check-catch

(check-catch 'type-error (json-schema-validator-validate 'not-a-validator 1))

(define person-schema
  '(("type" . "object")
    ("properties"
     ("name" ("type" . "string"))
     ("age" ("type" . "integer") ("minimum" . 0)))
    ("required" . #("name" "age"))
    ("additionalProperties" . #f))
)

(define person-validator
  (make-json-schema-validator person-schema '((check-schema? . #t)))
) ;define

(check (json-schema-validator-validate person-validator '(("name" . "A") ("age" . 1)))
  =>
  '(("name" . "A") ("age" . 1))
) ;check
(check-catch 'json-schema-validation-error
  (json-schema-validator-validate person-validator '(("name" . "A")))
) ;check-catch

(check-report)
