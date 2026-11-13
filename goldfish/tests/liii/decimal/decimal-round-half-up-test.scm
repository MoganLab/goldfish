;; decimal-round-half-up
;; 舍入模式常量：四舍五入（向最近邻居，距离相等时远离零）。
;;
;; 语法
;; ----
;; decimal-round-half-up
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; symbol?
;;   值为 'half-up。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check decimal-round-half-up => 'half-up)

;; 在 decimal-round 中使用
(check (decimal->string
        (decimal-round (string->decimal "1.25") 1 decimal-round-half-up))
       => "1.3"
) ;check

(check-report)
