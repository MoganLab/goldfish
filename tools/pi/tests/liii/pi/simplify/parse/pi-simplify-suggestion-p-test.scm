;; pi-simplify-suggestion?
;; 判断对象是否为 pi-simplify-suggestion 记录。
;;
;; 语法
;; ----
;; (pi-simplify-suggestion? x)
;;
;; 参数
;; ----
;; x : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 为 pi-simplify-suggestion 记录时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi simplify parse))

(check-set-mode! 'report-failed)

(check-true (pi-simplify-suggestion? (make-pi-simplify-suggestion "a.scm" 1 "cat" "msg" #f)))
(check-false (pi-simplify-suggestion? '()))
(check-false (pi-simplify-suggestion? #f))

(check-report)
