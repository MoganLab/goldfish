(import (liii check)
        (liii date-time tzdb))

(check-set-mode! 'report-failed)

;; reload-timezone-data!
;; 清空已缓存的时区对象和时区名称列表，重新加载时区数据。
;;
;; 语法
;; ----
;; (reload-timezone-data!)
;;
;; 示例
;; ----
;; (reload-timezone-data!)

(reload-timezone-data!)
(check-true (> (length (tz-timezones)) 0))

(check-report)
