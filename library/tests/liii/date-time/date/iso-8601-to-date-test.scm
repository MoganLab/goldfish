(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; iso-8601->date
;; 将符合 ISO-8601 格式的日期字符串解析为日期对象。
;;
;; 语法
;; ----
;; (iso-8601->date str)
;;
;; 参数
;; ----
;; str : string? 符合 ISO-8601 规范的日期字符串，如 "2020-01-01", "-0001-12-31"。
;;
;; 返回值
;; ----
;; date? 对应的日期对象。
;;
;; 示例
;; ----
;; (iso-8601->date "2020-01-01") => (make-date 2020 1 1)
;;
;; 错误处理
;; --------
;; type-error 当参数不是字符串或不是合法的 ISO-8601 日期时抛出错误。

(check-true (date=? (iso-8601->date "2020-01-01") (make-date 2020 1 1)))
(check-true (date=? (iso-8601->date "2020-11-12") (make-date 2020 11 12)))
(check-true (date=? (iso-8601->date "-0001-01-01") (make-date -1 1 1)))
(check-true (date=? (iso-8601->date "0000-01-01") (make-date 0 1 1)))

(check-catch 'type-error (iso-8601->date 20200101))
(check-catch 'type-error (iso-8601->date "invalid"))
(check-catch 'type-error (iso-8601->date "2020/01/01"))
(check-catch 'type-error (iso-8601->date "2020-02-30"))

(check-report)
