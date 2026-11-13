;; decimal-round
;; 将 decimal 按指定的 scale 和 rounding mode 进行舍入。
;;
;; 语法
;; ----
;; (decimal-round n scale rounding-mode)
;;
;; 参数
;; ----
;; n : decimal?
;;   要舍入的 decimal 值。
;; scale : integer?
;;   目标 scale，可以为任意整数。
;; rounding-mode : symbol?
;;   舍入模式，必须是 rounding mode 常量之一。
;;
;; 返回值
;; ----
;; decimal?
;;   按指定 scale 和 rounding mode 舍入后的 decimal 值。
;;
;; 说明
;; ----
;; 该函数类似于 JDK BigDecimal 的 setScale 方法，但要求显式指定 rounding mode。
;; 内部通过调用 decimal-set-scale 实现。
;; 负 scale 会将数值舍入到更高位（例如 -1 表示舍入到十位，-2 表示舍入到百位）。
;;
;; 错误处理
;; ----
;; type-error
;;   当 n 不是 decimal、scale 不是整数、或 rounding-mode 无效时抛出。

(import (liii check)
        (liii decimal)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)
(define (d str)
  (string->decimal str)
) ;define

;; half-up 舍入
(check (decimal->string (decimal-round (d "1.234") 2 decimal-round-half-up)) => "1.23")
(check (decimal->string (decimal-round (d "1.235") 2 decimal-round-half-up)) => "1.24")
(check (decimal->string (decimal-round (d "1.236") 2 decimal-round-half-up)) => "1.24")
(check (decimal->string (decimal-round (d "-1.235") 2 decimal-round-half-up)) => "-1.24")

;; half-down 舍入
(check (decimal->string (decimal-round (d "1.235") 2 decimal-round-half-down)) => "1.23")
(check (decimal->string (decimal-round (d "1.236") 2 decimal-round-half-down)) => "1.24")
(check (decimal->string (decimal-round (d "-1.235") 2 decimal-round-half-down)) => "-1.23")

;; half-even 舍入（银行家舍入）
(check (decimal->string (decimal-round (d "1.235") 2 decimal-round-half-even)) => "1.24")
(check (decimal->string (decimal-round (d "1.225") 2 decimal-round-half-even)) => "1.22")
(check (decimal->string (decimal-round (d "2.225") 2 decimal-round-half-even)) => "2.22")
(check (decimal->string (decimal-round (d "2.235") 2 decimal-round-half-even)) => "2.24")

;; up 舍入（远离零）
(check (decimal->string (decimal-round (d "1.231") 2 decimal-round-up)) => "1.24")
(check (decimal->string (decimal-round (d "-1.231") 2 decimal-round-up)) => "-1.24")
(check (decimal->string (decimal-round (d "1.230") 2 decimal-round-up)) => "1.23")

;; down 舍入（向零截断）
(check (decimal->string (decimal-round (d "1.239") 2 decimal-round-down)) => "1.23")
(check (decimal->string (decimal-round (d "-1.239") 2 decimal-round-down)) => "-1.23")

;; ceiling 舍入（向正无穷）
(check (decimal->string (decimal-round (d "1.231") 2 decimal-round-ceiling)) => "1.24")
(check (decimal->string (decimal-round (d "-1.239") 2 decimal-round-ceiling)) => "-1.23")

;; floor 舍入（向负无穷）
(check (decimal->string (decimal-round (d "1.239") 2 decimal-round-floor)) => "1.23")
(check (decimal->string (decimal-round (d "-1.231") 2 decimal-round-floor)) => "-1.24")

;; 零 scale 舍入到整数
(check (decimal->string (decimal-round (d "1.5") 0 decimal-round-half-up)) => "2")
(check (decimal->string (decimal-round (d "1.4") 0 decimal-round-half-up)) => "1")
(check (decimal->string (decimal-round (d "-1.5") 0 decimal-round-half-up)) => "-2")

;; 增加 scale（补零）
(check (decimal->string (decimal-round (d "1.2") 3 decimal-round-half-up)) => "1.200")

;; 保持相同 scale
(check (decimal->string (decimal-round (d "1.23") 2 decimal-round-half-up)) => "1.23")

;; unnecessary 模式：精确值不报错
(check (decimal->string (decimal-round (d "1.23") 2 decimal-round-unnecessary)) => "1.23")

;; 负 scale 舍入（舍入到十位、百位等）
(check (decimal->string (decimal-round (d "123.456") -1 decimal-round-half-up)) => "120")
(check (decimal->string (decimal-round (d "123.456") -2 decimal-round-half-up)) => "100")
(check (decimal->string (decimal-round (d "567.890") -2 decimal-round-half-up)) => "600")
(check (decimal->string (decimal-round (d "999.999") -3 decimal-round-half-up)) => "1000")

;; 形式 2: (decimal-round n math-context)（BUG-6）
(check (decimal->string
        (decimal-round (d "12345678.9") (make-math-context 7 'half-even)))
       => "12345680")
(check (decimal->string
        (decimal-round (d "1.2345678") (make-math-context 5 'half-up)))
       => "1.2346")
(check (decimal->string
        (decimal-round (d "1.2") (make-math-context 5 'half-up)))
       => "1.2")
(check (decimal->string
        (decimal-round (d "0.000") (make-math-context 5 'half-up)))
       => "0.000")

;; 错误处理
(check-catch 'type-error (decimal-round 1 2 decimal-round-half-up))
(check-catch 'type-error (decimal-round (d "1.2") 1.5 decimal-round-half-up))
(check-catch 'value-error (decimal-round (d "1.2") 2 'invalid-mode))
(check-catch 'value-error (decimal-round (d "1.234") 2 decimal-round-unnecessary))

(check-report)
