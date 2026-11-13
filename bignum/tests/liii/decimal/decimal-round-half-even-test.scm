;; decimal-round-half-even
;; 舍入模式常量：银行家舍入（向最近邻居，距离相等时向偶数）。
;;
;; 语法
;; ----
;; decimal-round-half-even
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; symbol?
;;   值为 'half-even。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check decimal-round-half-even => 'half-even)

;; 在 decimal-round 中使用：2.35 舍入到 2.4（4 为偶数）
(check (decimal->string
        (decimal-round (string->decimal "2.35") 1 decimal-round-half-even))
       => "2.4"
) ;check

(check-report)
