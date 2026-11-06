(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date->mjd
;; 将日期转换为简化儒略日（Modified Julian Day, MJD）。
;; MJD 0 对应公历 1858-11-17。
;;
;; 语法
;; ----
;; (date->mjd date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? MJD 整数日序。
;;
;; 示例
;; ----
;; (date->mjd (make-date 1858 11 17)) => 0
;; (date->mjd (make-date 2025 10 17)) => 60965
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date->mjd (make-date 1858 11 17)) => 0)
(check (date->mjd (make-date 2025 10 17)) => 60965)

(check-catch 'type-error (date->mjd "1858-11-17"))
(check-catch 'type-error (date->mjd 0))

(check-report)
