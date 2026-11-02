;; length>?
;; 判断列表长度是否大于给定整数。
;;
;; 语法
;; ----
;; (length>? list n)
;;
;; 参数
;; ----
;; list : list?
;; 要检查的列表。
;;
;; n : integer?
;; 要比较的长度。
;;
;; 返回值
;; ------
;; boolean?
;; 如果列表长度大于n，返回#t，否则返回#f。

(import (liii list) (liii check))

(check-set-mode! 'report-failed)

(check-true (length>? '(1 2 3 4 5) 3))
(check-false (length>? '(1 2) 3))
(check-false (length>? '() 0))

(check-true (length>? '(1) 0))
(check-false (length>? '() 1))

(check-false (length>? '(1 2 . 3) 2))
(check-true (length>? '(1 2 . 3) 1))

(check-report)
