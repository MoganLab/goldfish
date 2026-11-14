;; pi-review-finding?
;; 判断对象是否为 pi-review-finding 记录。
;;
;; 语法
;; ----
;; (pi-review-finding? x)
;;
;; 参数
;; ----
;; x : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 为 pi-review-finding 记录时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi review parse))

(check-set-mode! 'report-failed)

(check-true (pi-review-finding? (make-pi-review-finding "INFO" "a.scm" 1 "cat" "msg" #f)))
(check-false (pi-review-finding? '()))
(check-false (pi-review-finding? #f))

(check-report)
