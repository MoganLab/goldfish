;; calc-mjd->date
;; 将修正儒略日（MJD）换算为日期。
;;
;; 语法
;; ----
;; (calc-mjd->date mjd)
;;
;; 参数
;; ----
;; mjd : integer? 修正儒略日。
;;
;; 返回值
;; ----
;; date? 对应日期。

(import (liii check) (liii date-time date) (liii date-time internal date-rd))

(check-set-mode! 'report-failed)

(check
  (let-values (((y m d) (date-ymd (calc-mjd->date 51544))))
    (list y m d)
  ) ;let-values
  =>
  '(2000 1 1)
) ;check
(check
  (let-values (((y m d) (date-ymd (calc-mjd->date 40587))))
    (list y m d)
  ) ;let-values
  =>
  '(1970 1 1)
) ;check
(check-catch 'type-error (calc-mjd->date "1"))

(check-report)
