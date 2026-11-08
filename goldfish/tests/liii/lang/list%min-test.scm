;; list%min / list%max / list%min-option / list%max-option
;; 最小/最大值（对标 Scala min/max/minOption/maxOption）。
;;
;; 语法
;; ----
;; ($ xs :min)            ($ xs :max)
;; ($ xs :min-option)     ($ xs :max-option)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表，元素须可比较，不可为空。
;;
;; 返回值
;; ------
;; :min/:max —— number?，空集抛 value-error（提示改用 -option 形态）；
;; :min-option/:max-option —— (option n)，空集返回 (none)。

(import (liii check) (liii lang) (liii option) (liii error))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :min) => 1)
(check ($ '(1 2 3) :max) => 3)
(check ($ '(5) :min) => 5)
(check ($ '(3 1 2) :filter (> _ 1) :min) => 2)
(check-catch 'value-error ($ '() :min))
(check-catch 'value-error ($ '() :max))
(check ($ '(1 2 3) :min-option) => (option 1))
(check ($ '(1 2 3) :max-option) => (option 3))
(check ($ '() :min-option) => (none))
(check ($ '() :max-option) => (none))

(check-report)
