;; type-error?
;; 判定错误标签是否属于类型错误体系。
;;
;; 语法
;; ----
;; (type-error? err) -> boolean
;;
;; 参数
;; ----
;; err : symbol
;; 待判定的错误符号。
;;
;; 返回值
;; ----
;; boolean
;; 匹配 'type-error 或 'wrong-type-arg 时返回 #t，否则返回 #f。

(import (liii check)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

(check-true (type-error? 'type-error))
(check-true (type-error? 'wrong-type-arg))
(check-false (type-error? 'value-error))

(check-report)
