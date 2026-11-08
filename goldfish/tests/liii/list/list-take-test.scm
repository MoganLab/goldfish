;; list-take
;; Scala风格的take函数，对越界情况容忍。
;;
;; 语法
;; ----
;; (list-take lst n)
;;
;; 参数
;; ----
;; lst : list?
;; 源列表，从中提取元素。
;;
;; n : integer?
;; 要提取的元素数量。
;;
;; 返回值
;; ------
;; list
;; 包含指定数量元素的新列表，从原列表的开头开始计数。

(import (liii list) (liii check))

(check-set-mode! 'report-failed)

(check (list-take '(1 2 3 4 5) 3) => '(1 2 3))
(check (list-take '(1 2 3 4 5) 0) => '())
(check (list-take '(1 2 3 4 5) 5) => '(1 2 3 4 5))

(check (list-take '(1 2 3) -1) => '())
(check (list-take '(1 2 3) 10) => '(1 2 3))

(check (list-take '() 0) => '())
(check (list-take '() 5) => '())

(check-catch 'type-error (list-take "not a list" 2))
(check-catch 'type-error (list-take '(1 2 3) "not a number"))

(check-report)
