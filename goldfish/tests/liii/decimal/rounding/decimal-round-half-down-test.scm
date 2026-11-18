;; decimal-round-half-down
;; 舍入模式常量：向最近邻居舍入，距离相等时向零。
;;
;; 语法
;; ----
;; decimal-round-half-down
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; symbol?
;;   值为 'half-down。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check decimal-round-half-down => 'half-down)

;; 在 decimal-round 中使用
(check (decimal->string
        (decimal-round (string->decimal "1.25") 1 decimal-round-half-down))
       => "1.2"
) ;check

(check-report)
