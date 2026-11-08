;; list%index-of
;; 查找元素首次出现的下标（对标 Scala List.indexOf）。
;;
;; 语法
;; ----
;; ($ xs :index-of e)
;;
;; 参数
;; ----
;; e : any
;; 待查找元素，按 equal? 比较。
;;
;; 返回值
;; ------
;; integer?
;; 首次出现的下标（0 起）；不存在返回 -1。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :index-of 3) => 2)
(check ($ '(1 2 3) :index-of 9) => -1)
(check ($ '() :index-of 1) => -1)
(check ($ '(1 2 3 1) :index-of 1) => 0)
(check ($ '("a" "b") :index-of "b") => 1)

(check-report)
