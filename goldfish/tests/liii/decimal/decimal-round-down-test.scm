;; decimal-round-down
;; 舍入模式常量：向零方向舍入。
;;
;; 语法
;; ----
;; decimal-round-down
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; symbol?
;;   值为 'down。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check decimal-round-down => 'down)

;; 在 decimal-round 中使用
(check (decimal->string
        (decimal-round (string->decimal "1.29") 1 decimal-round-down))
       => "1.2"
) ;check

(check-report)
