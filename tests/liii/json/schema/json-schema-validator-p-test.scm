(import (liii check) (liii json schema) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-validator?
;; 判断对象是否为 JSON Schema 校验器。
;;
;; 语法
;; ----
;; (json-schema-validator? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象。
;;
;; 返回值
;; ----
;; boolean
;; 是校验器返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 只有 make-json-schema-validator 产生的记录才是校验器。
;;
;; 示例
;; ----
;; (json-schema-validator? (make-json-schema-validator #t)) => #t
;; (json-schema-validator? 'not-a-validator) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-schema-validator? (make-json-schema-validator #t)))
(check-true
  (json-schema-validator? (make-json-schema-validator '(("type" . "integer"))))
) ;check-true
(check-false (json-schema-validator? 'not-a-validator))
(check-false (json-schema-validator? #t))
(check-false (json-schema-validator? '()))

(check-report)
