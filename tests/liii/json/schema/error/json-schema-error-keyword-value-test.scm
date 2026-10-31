(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-keyword-value
;; 读取触发错误的关键字取值。
;;
;; 语法
;; ----
;; (json-schema-error-keyword-value err)
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
;; 关键字的值，例如 type 断言中的 "integer"；没有取值时为 #f。
;; 取值是 Schema 中的原始 JSON 值。
;;
;; 示例
;; ----
;; (json-schema-error-keyword-value (car (json-schema-errors 1 '(("type" . "integer"))))) => "integer"
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check (json-schema-error-keyword-value err) => "integer")
(check-false (json-schema-error-keyword-value (make-validation-error 'validation "msg" #f #f 1 #t '() '() '() #f)
             ) ;json-schema-error-keyword-value
) ;check-false

(check-report)
