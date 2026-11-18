;; decimal-scale-by-power-of-ten
;; 将 decimal 按 10 的幂次缩放，相当于移动小数点位置。
;;
;; 语法
;; ----
;; (decimal-scale-by-power-of-ten n shift)
;;
;; 参数
;; ----
;; n : decimal?
;;   要缩放的 decimal 值。
;; shift : integer?
;;   10 的幂次，正数向右移动，负数向左移动。
;;
;; 返回值
;; ----
;; decimal?
;;   缩放后的 decimal 值。
;;
;; 说明
;; ----
;; 该函数内部调用 decimal-move-point-right 实现。
;; shift 为正时，相当于乘以 10^shift；shift 为负时，相当于除以 10^|shift|。
;;
;; 错误处理
;; ----
;; type-error
;;   当 n 不是 decimal 或 shift 不是整数时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
(define (d str)
  (string->decimal str)
) ;define

(check (decimal->string (decimal-scale-by-power-of-ten (d "1.23") 2)) => "123")
(check (decimal->string (decimal-scale-by-power-of-ten (d "1.23") -2)) => "0.0123")
(check (decimal-scale (decimal-scale-by-power-of-ten
                       (decimal-set-scale (d "1.2") 3)
                       1))
       => 2
) ;check
(check (decimal->string (decimal-scale-by-power-of-ten (d "0") 4)) => "0")

(check-catch 'type-error (decimal-scale-by-power-of-ten 1 2))
(check-catch 'type-error (decimal-scale-by-power-of-ten (d "1.2") 1.5))

(check-report)
