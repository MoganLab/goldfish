(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-keyword
;; 读取触发错误的关键字。
;;
;; 语法
;; ----
;; (json-schema-error-keyword err)
;;
;; 参数
;; ----
;; err : json-schema-error
;; 错误对象。
;;
;; 返回值
;; ----
;; string | boolean
;;
;; 注意
;; ----
;; 触发错误的关键字名称，例如 "type"；没有具体关键字时为 #f。
;; 关键字可用于定位 Schema 中的问题位置。
;;
;; 示例
;; ----
;; (json-schema-error-keyword (car (json-schema-errors 1 '(("type" . "integer"))))) => "type"
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check (json-schema-error-keyword err) => "type")
(check-false (json-schema-error-keyword (make-validation-error 'validation "msg" #f #f 1 #t '() '() '() #f)
             ) ;json-schema-error-keyword
) ;check-false

(check-report)
