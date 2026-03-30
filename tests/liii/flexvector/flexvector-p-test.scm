(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector?
;; 检查对象是否为可变长向量（flexvector）。
;;
;; 语法
;; ----
;; (flexvector? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要检查的对象。
;;
;; 返回值
;; -----
;; 如果 obj 是 flexvector，返回 #t；否则返回 #f。
;;
(check-true (flexvector? (flexvector)))
(check-false (flexvector? '()))
(check-false (flexvector? "not a flexvector"))
(check-false (flexvector? 42))

(check-report)
