(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-kind
;; 读取错误类别。
;;
;; 语法
;; ----
;; (json-schema-error-kind err)
;;
;; 参数
;; ----
;; err : json-schema-error
;; 错误对象。
;;
;; 返回值
;; ----
;; symbol
;;
;; 注意
;; ----
;; 常见取值是 validation（实例校验失败）与 schema（Schema 非法）。
;; 错误类别用于区分失败原因。
;;
;; 示例
;; ----
;; (json-schema-error-kind (car (json-schema-errors 1 '(("type" . "integer"))))) => 'validation
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check (json-schema-error-kind err) => 'validation)
(check (json-schema-error-kind (make-validation-error 'schema "msg" #f #f 1 #t '() '() '() #f)
       ) ;json-schema-error-kind
  =>
  'schema
) ;check

(check-report)
