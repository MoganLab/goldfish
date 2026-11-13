;; decimal-round-up
;; 舍入模式常量：远离零方向舍入。
;;
;; 语法
;; ----
;; decimal-round-up
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; symbol?
;;   值为 'up。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check decimal-round-up => 'up)

;; 在 decimal-round 中使用
(check (decimal->string
        (decimal-round (string->decimal "1.21") 1 decimal-round-up))
       => "1.3"
) ;check

(check-report)
