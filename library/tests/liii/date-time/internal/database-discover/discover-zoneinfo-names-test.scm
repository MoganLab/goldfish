;; discover-zoneinfo-names
;; 枚举系统 zoneinfo 目录中可用的 IANA 时区名称。
;;
;; 语法
;; ----
;; (discover-zoneinfo-names)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; list? 时区名称字符串列表。

(import (liii check) (liii date-time internal database-discover))

(check-set-mode! 'report-failed)

(define names (discover-zoneinfo-names))

(check-true (list? names))
(check-true (> (length names) 0))
(check-true (if (member "UTC" names string=?) #t #f))
(check-true (string? (car names)))

(check-report)
