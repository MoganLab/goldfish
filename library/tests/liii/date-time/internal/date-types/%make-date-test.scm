;; %make-date
;; 不校验参数直接构造 date 记录。
;;
;; 语法
;; ----
;; (%make-date year month day)
;;
;; 参数
;; ----
;; year : integer? 年。
;; month : integer? 月。
;; day : integer? 日。
;;
;; 返回值
;; ----
;; date? 新建的日期对象；不校验字段合法性。

(import (liii check) (liii date-time internal date-types))

(check-set-mode! 'report-failed)

(define d (%make-date 2021 13 45))

(check-true (date? d))
(check (%date-year d) => 2021)
(check (%date-month d) => 13)
(check (%date-day d) => 45)

(check-report)
