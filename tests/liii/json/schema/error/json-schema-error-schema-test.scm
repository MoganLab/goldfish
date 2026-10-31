(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-schema
;; 读取触发错误位置的 Schema。
;;
;; 语法
;; ----
;; (json-schema-error-schema err)
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
;; 触发错误位置的 Schema 值。
;; Schema 值可能是对象或布尔值。
;;
;; 示例
;; ----
;; (pair? (json-schema-error-schema (car (json-schema-errors 1 '(("type" . "integer")))))) => #t
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check-true (pair? (json-schema-error-schema err)))
(check (json-schema-error-schema err) => '(("type" . "integer")))

(check-report)
