(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; odd?
;; 判断一个整数是否是奇数。
;;
;; 语法
;; ----
;; (odd? obj)
;;
;; 参数
;; ----
;; obj : integer?
;; 整数。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是整数类型，当其为奇数时返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; type-error
;; 如果参数不是整数类型
(check-true (odd? 1))
(check-false (odd? 0))
(check-catch 'type-error (odd? 1.0+1.0i))
(check-catch 'type-error (odd? 1.0))
(check-catch 'type-error (odd? 0.0))
(check-catch 'type-error (odd? #\A))
(check-catch 'type-error (odd? #t))
(check-catch 'type-error (odd? #f))
(check-report)
