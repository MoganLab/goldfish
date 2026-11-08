;; list%contains
;; 判断是否包含指定元素（对标 Scala List.contains）。
;;
;; 语法
;; ----
;; ($ xs :contains e)
;;
;; 参数
;; ----
;; e : any
;; 待查找元素，按 equal? 比较。
;;
;; 返回值
;; ------
;; boolean
;; 存在等值元素则 #t；空表为 #f。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :contains 2) => #t)
(check ($ '(1 2 3) :contains 9) => #f)
(check ($ '() :contains 1) => #f)
(check ($ '("a" "b") :contains "a") => #t)

(check-report)
