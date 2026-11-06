;; check-passed?
;; 检查是否全部断言成功且总成功断言数等于指定值。
;;
;; 语法
;; ----
;; (check-passed? expected-total-count)
;;
;; 参数
;; ----
;; expected-total-count : integer?
;; 期望的总成功检查数。
;;
;; 返回值
;; ----
;; boolean
;; 当且仅当失败数为 0 且成功数等于 expected-total-count 时返回 #t，否则返回 #f。

(import (srfi srfi-78))

(check-set-mode! 'summary)
(check-reset!)

(check (+ 1 1) => 2)
(check (+ 2 2) => 4)

(check (check-passed? 2) => #t)
(check (check-passed? 1) => #f)

(check-report)
