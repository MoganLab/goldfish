;; split-at
;; 将列表在给定位置拆分为两个部分。
;;
;; 语法
;; ----
;; (split-at list k)
;;
;; 参数
;; ----
;; list : list?
;; 要拆分的列表。
;;
;; k : integer?
;; 拆分位置，必须是非负整数且不超过列表长度。
;;
;; 返回值
;; ------
;; values
;; 包含两个值：前k个元素组成的新列表以及剩余元素组成的列表。
;;
;; 错误处理
;; --------
;; - value-error：当 k 为负数或超过列表长度时抛出。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (list (split-at '(1 2 3 4 5) 3)) => '((1 2 3) (4 5)))
(check (list (split-at '(1 2 3 4 5) 0)) => '(() (1 2 3 4 5)))
(check (list (split-at '(1 2 3 4 5) 5)) => '((1 2 3 4 5) ()))

(check-catch 'value-error (split-at '(1 2 3 4 5) 10))
(check-catch 'value-error (split-at '(1 2 3 4 5) -1))

(check (list (split-at '(1 2 3 4 . 5) 0)) => '(() (1 2 3 4 . 5)))
(check (list (split-at '(1 2 3 4 . 5) 3)) => '((1 2 3) (4 . 5)))
(check (list (split-at '(1 2 3 4 . 5) 4)) => '((1 2 3 4) 5))

(check-catch 'value-error (split-at '(1 2 3 4 . 5) 10))
(check-catch 'value-error (split-at '(1 2 3 4 . 5) -1))

(check (list (split-at '() 0)) => '(() ()))
(check-catch 'value-error (split-at '() 10))
(check-catch 'value-error (split-at '() -1))

(check-report)
