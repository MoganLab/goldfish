;; calc-rata-die->date
;; 将 raté die 天数换算为日期。
;;
;; 语法
;; ----
;; (calc-rata-die->date rd)
;;
;; 参数
;; ----
;; rd : integer? raté die 天数。
;;
;; 返回值
;; ----
;; date? 对应日期。

(import (liii check) (liii date-time date) (liii date-time internal date-rd))

(check-set-mode! 'report-failed)

(check-true (date? (calc-rata-die->date 1)))
(check
  (let-values (((y m d) (date-ymd (calc-rata-die->date 1))))
    (list y m d)
  ) ;let-values
  =>
  '(1 1 1)
) ;check
(check
  (let-values (((y m d) (date-ymd (calc-rata-die->date 719163))))
    (list y m d)
  ) ;let-values
  =>
  '(1970 1 1)
) ;check
(check
  (let-values (((y m d) (date-ymd (calc-rata-die->date -1))))
    (list y m d)
  ) ;let-values
  =>
  '(0 12 30)
) ;check
(check-catch 'type-error (calc-rata-die->date "1"))

(check-report)
