;; pi-model-entry?
;; 判断对象是否为 <pi-model-entry> 记录。
;;
;; 语法
;; ----
;; (pi-model-entry? obj) -> boolean

;;
;; 参数
;; ----
;; entry : pi-model-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base) (liii check) (liii pi code config types))

(check-set-mode! 'report-failed)

(check-true (pi-model-entry? (make-pi-model-entry)))
(check-false (pi-model-entry? '()))
(check-false (pi-model-entry? #f))
(check-false (pi-model-entry? "str"))

(check-report)
