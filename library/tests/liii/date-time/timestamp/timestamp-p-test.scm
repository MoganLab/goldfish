(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp?
;; 判断给定对象是否为 timestamp 对象。
;;
;; 语法
;; ----
;; (timestamp? obj)
;;
;; 参数
;; ----
;; obj : any
;;
;; 返回值
;; ----
;; boolean?
;;
;; 示例
;; ----
;; (timestamp? (make-timestamp 2020 1 1 0 0 0 (utc-timezone))) => #t

(check-true (timestamp? (make-timestamp 2020 1 1 0 0 0 (utc-timezone))))
(check-false (timestamp? "2020-01-01T00:00:00Z"))
(check-false (timestamp? #f))

(check-report)
