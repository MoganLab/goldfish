;; repeat-item
;; 构造包含 n 个相同元素的列表。
;;
;; 语法
;; ----
;; (repeat-item item n)
;;
;; 参数
;; ----
;; item : any
;; 重复的元素。
;;
;; n : integer?
;; 重复次数。
;;
;; 返回值
;; ----
;; list?
;; 包含 n 个 item 的列表。

(import (scheme base) (liii check) (liii tui box hstack width))

(check-set-mode! 'report-failed)

(check (repeat-item 'a 3) => '(a a a))
(check (repeat-item "x" 1) => '("x"))
(check (repeat-item 10 0) => '())

(check-report)
