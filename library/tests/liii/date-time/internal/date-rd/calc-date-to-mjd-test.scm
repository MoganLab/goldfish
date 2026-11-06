;; calc-date->mjd
;; 将日期换算为修正儒略日（MJD）。
;;
;; 语法
;; ----
;; (calc-date->mjd date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 修正儒略日。

(import (liii check) (liii date-time date) (liii date-time internal date-rd))

(check-set-mode! 'report-failed)

(check (calc-date->mjd (make-date 2000 1 1)) => 51544)
(check (calc-date->mjd (make-date 1970 1 1)) => 40587)
(check-catch 'type-error (calc-date->mjd "date"))

(check-report)
