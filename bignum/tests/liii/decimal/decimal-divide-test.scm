;; decimal-divide
;; 计算两个 decimal 的商，按指定 scale 与舍入模式舍入。
;;
;; 语法
;; ----
;; (decimal-divide a b scale rounding-mode)
;;
;; 参数
;; ----
;; a : decimal?
;;   被除数。
;; b : decimal?
;;   除数。
;; scale : integer?
;;   目标 scale，可为负数。
;; rounding-mode : symbol?
;;   舍入模式。
;;
;; 返回值
;; ----
;; decimal?
;;   按指定 scale 与模式舍入后的商。
;;
;; 错误处理
;; ----
;; value-error
;;   当除数为零、舍入模式无效或必要舍入被禁止时抛出。
;; type-error
;;   当参数类型不正确时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (decimal->string (decimal-divide (d "1") (d "4") 2 decimal-round-half-up))
       => "0.25"
) ;check
(check (decimal->string (decimal-divide (d "1") (d "8") 2 decimal-round-half-up))
       => "0.13"
) ;check
(check (decimal->string (decimal-divide (d "1") (d "8") 2 decimal-round-down))
       => "0.12"
) ;check
(check (decimal->string (decimal-divide (d "-1") (d "8") 2 decimal-round-ceiling))
       => "-0.12"
) ;check
(check (decimal->string (decimal-divide (d "-1") (d "8") 2 decimal-round-floor))
       => "-0.13"
) ;check
(check (decimal->string (decimal-divide (d "1.00") (d "4") 2))
       => "0.25"
) ;check

(check-catch 'value-error (decimal-divide (d "1") (d "3") 2))
(check-catch 'value-error (decimal-divide (d "1") (d "0") 2 decimal-round-half-up))
(check-catch 'value-error (decimal-divide (d "1") (d "2") 2 'bad-mode))
(check-catch 'type-error (decimal-divide 1 (d "2") 2 decimal-round-half-up))

;; 负 scale 支持与舍入信息保留（BUG-1）
(check (decimal->string (decimal-divide (d "1") (d "2") -1 decimal-round-half-up))
       => "0")
(check (decimal->string (decimal-divide (d "100") (d "1") -1 decimal-round-half-up))
       => "100")
(check (decimal->string (decimal-divide (d "150") (d "1") -2 decimal-round-half-up))
       => "200")
(check (decimal->string (decimal-divide (d "15") (d "1") -1 decimal-round-half-up))
       => "20")
(check (decimal->string (decimal-divide (d "2.51") (d "1") 0 decimal-round-half-up))
       => "3")
(check (decimal->string (decimal-divide (d "1.5") (d "1") 0 decimal-round-half-even))
       => "2")

(check-report)
