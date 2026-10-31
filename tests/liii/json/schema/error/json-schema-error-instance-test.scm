(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-instance
;; 读取触发错误位置的实例值。
;;
;; 语法
;; ----
;; (json-schema-error-instance err)
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
;; 触发错误位置的实例值。
;; 实例值是 JSON 数据本身，不是 Schema。
;;
;; 示例
;; ----
;; (json-schema-error-instance (car (json-schema-errors 1 '(("type" . "integer"))))) => 1
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check (json-schema-error-instance err) => "x")
(check
  (json-schema-error-instance
    (car (json-schema-errors "x" '(("type" . "integer"))))
  ) ;json-schema-error-instance
  =>
  "x"
) ;check

(check-report)
