;; mjd->date
;; 将简化儒略日（MJD）转换为公历日期对象。
;;
;; 语法
;; ----
;; (mjd->date mjd)
;;
;; 参数
;; ----
;; mjd : integer? MJD 整数日序。
;;
;; 返回值
;; ----
;; date? 对应的日期对象。
;;
;; 错误处理
;; --------
;; type-error 当参数不是整数时抛出错误。

(import (liii check) (liii date-time date))

(check-set-mode! 'report-failed)


(check-true (date=? (mjd->date 0) (make-date 1858 11 17)))
(check-true (date=? (mjd->date 60965) (make-date 2025 10 17)))

(check-catch 'type-error (mjd->date 1.5))
(check-catch 'type-error (mjd->date "0"))
(check-catch 'type-error (mjd->date 'foo))

(check-report)
