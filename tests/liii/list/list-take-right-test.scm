;; list-take-right
;; Scala风格的take-right函数，对越界情况容忍。
;;
;; 语法
;; ----
;; (list-take-right lst n)
;;
;; 参数
;; ----
;; lst : list?
;; 源列表，从中提取元素。
;;
;; n : integer?
;; 要从尾部提取的元素数量。
;;
;; 返回值
;; ------
;; list
;; 包含从尾部开始指定数量元素的新列表。

(import (liii list) (liii check))

(check-set-mode! 'report-failed)

(check (list-take-right '(1 2 3 4 5) 3) => '(3 4 5))
(check (list-take-right '(1 2 3 4 5) 0) => '())
(check (list-take-right '(1 2 3 4 5) 5) => '(1 2 3 4 5))

(check (list-take-right '(1 2 3) -1) => '())
(check (list-take-right '(1 2 3) 10) => '(1 2 3))

(check (list-take-right '() 0) => '())

(check-catch 'type-error (list-take-right "not a list" 2))
(check-catch 'type-error (list-take-right '(1 2 3) "not a number"))

(check-report)
