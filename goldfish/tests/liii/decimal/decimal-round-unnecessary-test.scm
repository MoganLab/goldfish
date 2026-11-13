;; decimal-round-unnecessary
;; 舍入模式常量：断言无需舍入，需要舍入时报错。
;;
;; 语法
;; ----
;; decimal-round-unnecessary
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; symbol?
;;   值为 'unnecessary。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check decimal-round-unnecessary => 'unnecessary)

;; 精确时正常返回
(check (decimal->string
        (decimal-round (string->decimal "1.2") 1 decimal-round-unnecessary))
       => "1.2"
) ;check

(check-report)
