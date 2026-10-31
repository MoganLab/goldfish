(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-cause
;; 读取错误的底层原因。
;;
;; 语法
;; ----
;; (json-schema-error-cause err)
;;
;; 参数
;; ----
;; err : json-schema-error
;; 错误对象。
;;
;; 返回值
;; ----
;; any?
;;
;; 注意
;; ----
;; 底层原因；没有时为 #f。
;; 原因用于串联内部错误与对外报告的错误。
;;
;; 示例
;; ----
;; (json-schema-error-cause (car (json-schema-errors 1 '(("type" . "integer"))))) => #f
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check-false (json-schema-error-cause err))

(define cause (list 'underlying))

(define error
  (make-validation-error 'schema
    "parent"
    "anyOf"
    #()
    1
    #t
    '()
    '("anyOf")
    '()
    cause
  ) ;make-validation-error
) ;define
(check (json-schema-error-cause error) => cause)

(check-report)
