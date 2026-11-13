;; decimal-equals?
;; 同时比较数值和 scale（JDK 的 equals 语义）。
;;
;; 语法
;; ----
;; (decimal-equals? a b)
;;
;; 参数
;; ----
;; a : decimal?
;;   第一个操作数。
;; b : decimal?
;;   第二个操作数。
;;
;; 返回值
;; ----
;; boolean?
;;   数值与 scale 都相同返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 与 decimal=? 不同：1.0 和 1.00 数值相等但 scale 不同，所以不相等。
;;
;; 错误处理
;; ----
;; type-error
;;   当任一参数不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

;; 数值和 scale 都相同
(check (decimal-equals? (string->decimal "1.5") (string->decimal "1.5")) => #t)
(check (decimal-equals? (string->decimal "0") (string->decimal "0")) => #t)
(check (decimal-equals? (string->decimal "-123.456")
                        (string->decimal "-123.456")) => #t)

;; 数值相同但 scale 不同
(check (decimal-equals? (string->decimal "1.5") (string->decimal "1.50")) => #f)
(check (decimal-equals? (string->decimal "123") (string->decimal "123.0")) => #f)

;; 与 decimal=? 的对比
(check (decimal=? (string->decimal "1.5") (string->decimal "1.50")) => #t)

(check-catch 'type-error (decimal-equals? "1.5" (string->decimal "1.5")))

(check-report)
