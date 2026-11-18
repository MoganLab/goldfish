;; decimal-round-floor
;; 舍入模式常量：向负无穷方向舍入。
;;
;; 语法
;; ----
;; decimal-round-floor
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; symbol?
;;   值为 'floor。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check decimal-round-floor => 'floor)

;; 在 decimal-round 中使用
(check (decimal->string
        (decimal-round (string->decimal "1.29") 1 decimal-round-floor))
       => "1.2"
) ;check

(check-report)
