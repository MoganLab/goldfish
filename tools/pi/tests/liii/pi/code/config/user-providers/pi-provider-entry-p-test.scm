;; pi-provider-entry?
;; 判断对象是否为 <pi-provider-entry> 记录。
;;
;; 语法
;; ----
;; (pi-provider-entry? obj) -> boolean

;;
;; 参数
;; ----
;; entry : pi-provider-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base) (liii check) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(check-true (pi-provider-entry? (make-pi-provider-entry)))
(check-false (pi-provider-entry? '()))
(check-false (pi-provider-entry? #f))
(check-false (pi-provider-entry? "str"))

(check-report)
