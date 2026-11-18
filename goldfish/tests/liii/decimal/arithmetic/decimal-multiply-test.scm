;; decimal-multiply
;; 计算两个 decimal 的积。
;;
;; 语法
;; ----
;; (decimal-multiply a b)
;;
;; 参数
;; ----
;; a : decimal?
;;   第一个乘数。
;; b : decimal?
;;   第二个乘数。
;;
;; 返回值
;; ----
;; decimal?
;;   a 与 b 的积，结果 scale = 左操作数 scale + 右操作数 scale。
;;
;; 错误处理
;; ----
;; type-error
;;   当任一参数不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

;; 乘法保留 scale：结果 scale = 左操作数 scale + 右操作数 scale
(check (decimal->string (decimal-multiply (d "1.20") (d "0.50"))) => "0.6000")
(check (decimal-scale (decimal-multiply (d "1.20") (d "0.50"))) => 4)

(check (decimal->string (decimal-multiply (d "-3") (d "0.25"))) => "-0.75")
(check (decimal-scale (decimal-multiply (d "-3") (d "0.25"))) => 2)

(check (decimal->string (decimal-abs (decimal-multiply (d "-2.5") (d "4")))) => "10.0")
(check (decimal-scale (decimal-abs (decimal-multiply (d "-2.5") (d "4")))) => 1)

(check-catch 'type-error (decimal-multiply 1 (d "1")))

(check-report)
