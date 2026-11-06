;; calc-date->rata-die
;; 将日期换算为 raté die 天数。
;;
;; 语法
;; ----
;; (calc-date->rata-die date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? raté die 天数（0001-01-01 为 1）。

(import (liii check) (liii date-time date) (liii date-time internal date-rd))

(check-set-mode! 'report-failed)

(check (calc-date->rata-die (make-date 1 1 1)) => 1)
(check (calc-date->rata-die (make-date 1970 1 1)) => 719163)
(check (calc-date->rata-die (make-date 2000 1 1))
  =>
  (+ (calc-date->rata-die (make-date 1999 12 31)) 1)
) ;check
(check-catch 'type-error (calc-date->rata-die "date"))

(check-report)
