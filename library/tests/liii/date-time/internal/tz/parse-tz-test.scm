;; parse-tz
;; 解析 POSIX TZ 字符串为 tz-string 记录。
;;
;; 语法
;; ----
;; (parse-tz input)
;;
;; 参数
;; ----
;; input : string? 或 bytevector? POSIX TZ 定义。
;;
;; 返回值
;; ----
;; tz-string? 解析得到的时区定义记录。

(import (liii check) (liii date-time internal tz))

(check-set-mode! 'report-failed)

(define est (parse-tz "EST5EDT,M3.2.0,M11.1.0"))

(check (tz-string-stdname est) => "EST")
(check (tz-offset-hour (tz-string-stdoffset est)) => 5)
(check (tz-string-dstname est) => "EDT")
(check (tz-offset-hour (tz-string-dstoffset est)) => 4)
(check-true (tz-transition? (tz-string-change-to-dst est)))
(check-true (tz-transition? (tz-string-change-to-std est)))
(check (tz-string-stdname (parse-tz "UTC0")) => "UTC")
(check (tz-string-dstname (parse-tz "UTC0")) => #f)
(check (tz-string-stdname (parse-tz (string->utf8 "EST5EDT"))) => "EST")
(check-catch 'type-error (parse-tz 123))

(check-report)
