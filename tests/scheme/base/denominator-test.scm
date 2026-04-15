(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; denominator
;; 返回有理数的分母部分。
;;
;; 语法
;; ----
;; (denominator q)
;;
;; 参数
;; ----
;; q : rational?
;; 有理数。
;;
;; 返回值
;; ------
;; integer?
;; 返回有理数的分母部分。
;; 对于整数，分母是1；对于有理数a/b，返回b，b总是正整数。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是有理数时抛出错误。
;; denominator测试
(check (denominator 1/2) => 2)
(check (denominator 4/5) => 5)
(check (denominator -3/7) => 7)
(check (denominator 5) => 1)
(check (denominator 0) => 1)
(check (denominator (inexact->exact 2.5)) => 2)
;; 补充denominator测试
(check (denominator 42) => 1)
(check (denominator -42) => 1)
(check (denominator 1/3) => 3)
(check (denominator 2) => 1)
(check (denominator -1/2) => 2)
(check (denominator (inexact->exact 5.5)) => 2)
(check (denominator (inexact->exact 0.25)) => 4)
(check-report)