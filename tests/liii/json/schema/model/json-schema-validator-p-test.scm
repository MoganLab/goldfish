;; json-schema-validator?
;; 判断对象是否为校验器记录。
;;
;; 语法
;; ----
;; (json-schema-validator? value)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是校验器记录返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 校验器记录由 make-validator 创建。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema model))

(check-set-mode! 'report-failed)

(check-true (json-schema-validator? (make-validator #t '())))
(check-true
  (json-schema-validator? (make-validator '(("type" . "integer")) '()))
) ;check-true

;; 其它对象不是校验器记录。

(check-false (json-schema-validator? '()))
(check-false (json-schema-validator? #t))
(check-false (json-schema-validator? "validator"))

(check-report)
