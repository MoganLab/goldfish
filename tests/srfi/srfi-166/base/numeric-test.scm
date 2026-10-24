(import (liii check)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; numeric
;; 数值格式化组合子，支持基数（2~36）、自定义精度、千分位逗号规则、符号规则和小数点对齐。
;;
;; 语法
;; ----
;; (numeric n [radix] [precision] [sign-rule] [comma-rule] [comma-sep] [decimal-sep])
;; (numeric/comma n [comma-rule])
;; (numeric/si n [base] [separator])
;; (numeric/fitted width n [radix] [precision] ...)
;;
;; 示例
;; ----
;; (show #f (numeric 100)) => "100"
;; (show #f (numeric 2 2)) => "10"
;; (show #f (with ((radix 16)) #xDEAD)) => "#xdead"
;;
;; 另见
;; ----
;; (liii format base) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format base))
;;   (show #f (numeric 100))

(check (show #f -1) => "-1")
(check (show #f 0) => "0")
(check (show #f 100) => "100")
(check (show #f (numeric 100)) => "100")
(check (show #f (with ((radix 16)) #xDEAD)) => "#xdead")
(check (show #f (with ((precision 2)) 3.14159)) => "3.14")
(check (show #f (with ((precision 2)) 3.0)) => "3.00")
(check (show #f (with ((decimal-align 5)) (numeric 3.14159))) => "   3.14159")
(check (show #f (with ((comma-rule 3)) (numeric 299792458))) => "299,792,458")
(check (show #f (with ((comma-rule 3) (comma-sep #\.)) (numeric 299792458))) => "299.792.458")
(check (show #f (numeric 2 2)) => "10")
(check (show #f (numeric 9 2)) => "1001")
(check (show #f (numeric 9/8 10 2)) => "1.12")
(check (show #f (numeric 9/8 10 4)) => "1.1250")
(check (show #f (numeric/si 608)) => "608")
(check (show #f (numeric/si 3986)) => "4k")
(check (show #f (numeric/comma 1234567)) => "1,234,567")
(check (show #f (numeric/fitted 4 1.2345 10 2)) => "1.23")

(check-report)
