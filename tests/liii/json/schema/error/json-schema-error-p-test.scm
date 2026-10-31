(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error?
;; 判断对象是否为 JSON Schema 错误对象。
;;
;; 语法
;; ----
;; (json-schema-error? value)
;;
;; 参数
;; ----
;; value : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;;
;; 注意
;; ----
;; 是错误对象返回 #t，否则返回 #f。
;; 错误对象是 <json-schema-error> 记录。
;;
;; 示例
;; ----
;; (json-schema-error? (car (json-schema-errors 1 '(("type" . "integer"))))) => #t
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-schema-error? (make-validation-error 'validation "msg" #f #f 1 #t '() '() '() #f)
            ) ;json-schema-error?
) ;check-true
(check-false (json-schema-error? 'not-an-error))
(check-false (json-schema-error? #t))
(check-false (json-schema-error? '()))

(check-report)
