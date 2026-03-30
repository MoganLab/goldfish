(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; even?
;; 判断一个整数是否是偶数。
;;
;; 语法
;; ----
;; (even? obj)
;;
;; 参数
;; ----
;; obj : integer?
;; 整数。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是整数类型，当其为偶数时返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果参数不是整数类型

(check-true (even? 0))
(check-false (even? 1))

(check-catch 'wrong-type-arg (even? 0.0))
(check-catch 'wrong-type-arg (even? 1.0))
(check-catch 'wrong-type-arg (even? 1+i))
(check-catch 'wrong-type-arg (even? #\A))
(check-catch 'wrong-type-arg (even? #t))
(check-catch 'wrong-type-arg (even? #f))

(check-report)
