;; decimal-round-ceiling
;; 舍入模式常量：向正无穷方向舍入。
;;
;; 语法
;; ----
;; decimal-round-ceiling
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; symbol?
;;   值为 'ceiling。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check decimal-round-ceiling => 'ceiling)

;; 在 decimal-round 中使用
(check (decimal->string
        (decimal-round (string->decimal "1.21") 1 decimal-round-ceiling))
       => "1.3"
) ;check

(check-report)
