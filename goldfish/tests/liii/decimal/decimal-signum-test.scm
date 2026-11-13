;; decimal-signum
;; 返回 decimal 的符号。
;;
;; 语法
;; ----
;; (decimal-signum n)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;;
;; 返回值
;; ----
;; integer?
;;   -1 表示负数，0 表示零，1 表示正数。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (decimal-signum (d "0")) => 0)
(check (decimal-signum (d "0.000")) => 0)
(check (decimal-signum (d "12.3")) => 1)
(check (decimal-signum (d "-12.3")) => -1)

(check-catch 'type-error (decimal-signum 1))

(check-report)
