;; decimal-move-point-right
;; 将 decimal 小数点向右移动 n 位。
;;
;; 语法
;; ----
;; (decimal-move-point-right n count)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;; count : integer?
;;   移动位数，可为负数（相当于向左移动）。
;;
;; 返回值
;; ----
;; decimal?
;;   移动小数点后的 decimal。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数类型不正确时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (decimal->string (decimal-move-point-right (d "123.45") 2)) => "12345")
(check (decimal->string (decimal-move-point-right (d "1.2") 5)) => "120000")
(check (decimal->string (decimal-move-point-right (d "1.2") -2)) => "0.012")
(check (decimal-scale (decimal-move-point-right
                       (decimal-set-scale (d "1.2") 3)
                       1))
       => 2
) ;check

(check-catch 'type-error (decimal-move-point-right (d "1.2") 1.5))

(check-report)
