(import (liii check)
        (liii date-time gregorian))

(check-set-mode! 'report-failed)

;; day-name->weekday
;; 将星期符号转换为 ISO 星期序号（1-7，周一为 1，周日为 7）。
;;
;; 语法
;; ----
;; (day-name->weekday name)
;;
;; 参数
;; ----
;; name : symbol? 星期符号，取值为 'monday, 'tuesday, 'wednesday, 'thursday, 'friday, 'saturday, 'sunday。
;;
;; 返回值
;; ----
;; integer? 1 到 7 的整数。
;;
;; 示例
;; ----
;; (day-name->weekday 'monday) => 1
;; (day-name->weekday 'sunday) => 7
;;
;; 错误处理
;; --------
;; type-error 当参数不是有效星期符号时抛出错误。

(check (day-name->weekday 'monday) => 1)
(check (day-name->weekday 'tuesday) => 2)
(check (day-name->weekday 'wednesday) => 3)
(check (day-name->weekday 'thursday) => 4)
(check (day-name->weekday 'friday) => 5)
(check (day-name->weekday 'saturday) => 6)
(check (day-name->weekday 'sunday) => 7)

(check-catch 'type-error (day-name->weekday 'invalid))
(check-catch 'type-error (day-name->weekday "monday"))
(check-catch 'type-error (day-name->weekday 1))

(check-report)
