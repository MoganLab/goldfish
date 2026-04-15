(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; exact?
;; 判断一个数是否是精确数。
;;
;; 语法
;; ----
;; (exact? obj)
;;
;; 参数
;; ----
;; obj : number?
;; 任意数值类型的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是精确数（整数、有理数、精确浮点数）返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; 无错误情况。
(check-true (exact? 1))
(check-true (exact? 1/2))
(check-false (exact? 0.3))
(check-report)