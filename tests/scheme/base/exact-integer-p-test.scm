(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; exact-integer?
;; 判断一个数值是否为精确整数。
;;
;; 语法
;; ----
;; (exact-integer? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是精确数值且为整数返回 #t，否则返回 #f。
(check-true (exact-integer? 42))
(check-true (exact-integer? -42))
(check-true (exact-integer? 0))
(check-false (exact-integer? 42.0))
(check-false (exact-integer? 1/2))
(check-false (exact-integer? 3.14))
(check-false (exact-integer? 1.0+2.0i))
(check-false (exact-integer? "42"))
(check-false (exact-integer? #t))
(check-false (exact-integer? 'symbol))
(check-report)