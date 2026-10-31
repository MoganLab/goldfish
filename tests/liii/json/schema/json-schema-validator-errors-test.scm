(import (liii check) (liii json schema) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-validator-errors
;; 用校验器收集实例不满足 Schema 时的全部错误。
;;
;; 语法
;; ----
;; (json-schema-validator-errors validator instance)
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
;; list
;; 错误对象列表，按发现顺序排列；校验通过时返回空列表。
;;
;; 注意
;; ----
;; 与 json-schema-errors 相同，但复用校验器已初始化的引用根与缓存。
;; 错误上限由 make-json-schema-validator 的 max-errors 选项决定。
;;
;; 示例
;; ----
;; (length (json-schema-validator-errors validator '(("name" . 1)))) => 1
;;
;; 错误处理
;; ----
;; type-error 当 validator 不是校验器时。

(define validator
  (make-json-schema-validator
    '(("type" . "object")
      ("properties" ("name" ("type" . "string")))
      ("required" . #("name")))
    '((check-schema? . #t)))
) ;define

(check (length (json-schema-validator-errors validator '(("name" . 1)))) => 1)
(check (length (json-schema-validator-errors validator '(("name" . "Liii")))) => 0)

;; max-errors 限制错误列表长度
(define limited
  (make-json-schema-validator
    '(("type" . "object")
      ("properties"
       ("name" ("type" . "string"))
       ("age" ("type" . "integer") ("minimum" . 0)))
      ("required" . #("name" "age")))
    '((max-errors . 1)))
) ;define
(check (length (json-schema-validator-errors limited '(("name" . 7)))) => 1)

;; 校验器可被反复复用，错误列表每次重新计算
(check (length (json-schema-validator-errors validator '(("name" . 1)))) => 1)
(check (length (json-schema-validator-errors validator '(("name" . "Liii")))) => 0)

(check-catch 'type-error (json-schema-validator-errors 'not-a-validator 1))

(check-report)
