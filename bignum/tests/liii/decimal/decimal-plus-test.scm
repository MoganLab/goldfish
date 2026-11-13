;; decimal-plus
;; 返回与输入相同的 decimal 值，保留 scale。
;;
;; 语法
;; ----
;; (decimal-plus n)
;;
;; 参数
;; ----
;; n : decimal?
;;   一个 decimal 值。
;;
;; 返回值
;; ----
;; decimal?
;;   返回 n 本身，scale 保持不变。
;;
;; 说明
;; ----
;; 该函数类似于 BigDecimal 的 plus() 方法，用于保留 scale 的上下文。
;; 对于正数，效果与原值相同；对于负数，不会取绝对值。
;;
;; 错误处理
;; ----
;; type-error
;;   当输入不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
(define (d str)
  (string->decimal str)
) ;define

(check (decimal->string (decimal-plus (d "1.23"))) => "1.23")
(check (decimal-scale (decimal-plus (decimal-set-scale (d "1.2") 3))) => 3)
(check (decimal->string (decimal-plus (decimal-set-scale (d "0") 4))) => "0.0000")
(check (decimal-scale (decimal-plus (decimal-set-scale (d "0") 4))) => 4)

(check-catch 'type-error (decimal-plus 1))

(check-report)
