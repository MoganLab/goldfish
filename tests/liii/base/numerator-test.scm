(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; numerator
;; 返回有理数的分子部分。
;;
;; 语法
;; ----
;; (numerator q)
;;
;; 参数
;; ----
;; q : rational?
;; 有理数。
;;
;; 返回值
;; ------
;; integer?
;; 返回有理数的分子部分。
;; 对于整数，分子是整数本身；对于有理数a/b，返回a。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是有理数时抛出错误。

;; numerator测试
(check (numerator 1/2) => 1)
(check (numerator 4/5) => 4)
(check (numerator -3/7) => -3)
(check (numerator 5) => 5)
(check (numerator 0) => 0)
(check (numerator (inexact->exact 2.5)) => 5)

;; 补充numerator测试
(check (numerator 42) => 42)
(check (numerator -42) => -42)
(check (numerator 1/3) => 1)
(check (numerator 10/5) => 2)
(check (numerator -4/8) => -1)
(check (numerator 0) => 0)

(check-report)
