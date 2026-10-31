(import (liii check) (liii json schema) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-validator-valid?
;; 用校验器判断实例是否满足 Schema，遇到首个错误即停止评估。
;;
;; 语法
;; ----
;; (json-schema-validator-valid? validator instance)
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
;; boolean
;; 满足 Schema 返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 该函数在首个错误后短路，不收集全部错误；
;; 需要完整错误列表时请使用 json-schema-validator-errors。
;;
;; 示例
;; ----
;; (json-schema-validator-valid? validator 1) => #t
;;
;; 错误处理
;; ----
;; type-error 当 validator 不是校验器时。

(define validator
  (make-json-schema-validator
    '(("type" . "object")
      ("properties" ("name" ("type" . "string")))
      ("required" . #("name")))
    '((check-schema? . #t))
  ) ;make-json-schema-validator
) ;define

(check-true (json-schema-validator-valid? validator '(("name" . "Liii"))))
(check-false (json-schema-validator-valid? validator '(("name" . 1))))
(check-false (json-schema-validator-valid? validator '()))

;; 校验器可被反复复用

(define reusable (make-json-schema-validator '(("type" . "integer"))))
(check-true (json-schema-validator-valid? reusable 1))
(check-true (json-schema-validator-valid? reusable 2))
(check-false (json-schema-validator-valid? reusable "2"))

(check-catch 'type-error (json-schema-validator-valid? 'not-a-validator 1))

(define person-schema
  '(("type" . "object")
    ("properties"
     ("name" ("type" . "string"))
     ("age" ("type" . "integer") ("minimum" . 0)))
    ("required" . #("name" "age"))
    ("additionalProperties" . #f))
) ;define

(define person-validator
  (make-json-schema-validator person-schema '((check-schema? . #t)))
) ;define

(check-true (json-schema-validator-valid? person-validator '(("name" . "A")
                                                             ("age" . 1)))
) ;check-true
(check-false (json-schema-validator-valid? person-validator '(("name" . "A"))))

(check-report)
