;; decimal-set-scale
;; 设置 decimal 的 scale，需要舍入时按指定模式处理。
;;
;; 语法
;; ----
;; (decimal-set-scale n scale)
;; (decimal-set-scale n scale rounding-mode)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;; scale : integer?
;;   目标 scale，可为负数（舍入到十位、百位等）。
;; rounding-mode : symbol?
;;   可选的舍入模式，默认 decimal-round-unnecessary。
;;
;; 返回值
;; ----
;; decimal?
;;   指定 scale 的新 decimal。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数类型不正确时抛出。
;; value-error
;;   当需要舍入但模式为 unnecessary 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (decimal->string (decimal-set-scale (d "1.2") 3)) => "1.200")
(check (decimal-scale (decimal-set-scale (d "1.2") 3)) => 3)
(check (decimal->string (decimal-set-scale (d "1.25") 1 decimal-round-half-up))
       => "1.3"
) ;check
(check (decimal->string (decimal-set-scale (d "1.25") 1 decimal-round-half-even))
       => "1.2"
) ;check
(check (decimal->string (decimal-set-scale (d "-1.25") 1 decimal-round-floor))
       => "-1.3"
) ;check
(check (decimal->string (decimal-set-scale (d "-1.25") 1 decimal-round-ceiling))
       => "-1.2"
) ;check

;; 负 scale 支持（舍入到十位、百位等）
(check (decimal->string (decimal-set-scale (d "123") -1 decimal-round-half-up)) => "120")
(check (decimal->string (decimal-set-scale (d "1234") -2 decimal-round-half-up)) => "1200")

(check-catch 'type-error (decimal-set-scale 1 2))

(check-report)
