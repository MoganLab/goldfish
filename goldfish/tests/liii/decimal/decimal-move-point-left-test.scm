;; decimal-move-point-left
;; 将 decimal 小数点向左移动 n 位。
;;
;; 语法
;; ----
;; (decimal-move-point-left n count)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;; count : integer?
;;   移动位数，可为负数（相当于向右移动）。
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

(check (decimal->string (decimal-move-point-left (d "123.45") 2)) => "1.2345")
(check (decimal->string (decimal-move-point-left (d "1.2") -2)) => "120")

(check-catch 'type-error (decimal-move-point-left 1 2))

(check-report)
