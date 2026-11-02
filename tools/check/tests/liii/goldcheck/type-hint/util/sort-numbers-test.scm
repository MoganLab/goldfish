;; sort-numbers
;; 数字列表升序排序。
;;
;; 语法
;; ----
;; (sort-numbers l)
;;
;; 参数
;; ----
;; l : list of number  待排序的数字列表
;;
;; 返回值
;; ------
;; list of number
;; 升序排列后的新数字列表。

(import (scheme base) (liii check) (liii goldcheck type-hint util))

(check-set-mode! 'report-failed)

;; 1. 乱序数字排序
(check (sort-numbers '(3 1 4 1 5 9 2 6)) => '(1 1 2 3 4 5 6 9))

;; 2. 已有序列表
(check (sort-numbers '(1 2 3)) => '(1 2 3))

;; 3. 空列表
(check (sort-numbers '()) => '())

(check-report)
