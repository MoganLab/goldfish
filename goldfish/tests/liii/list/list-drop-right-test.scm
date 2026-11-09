;; list-drop-right
;; Scala风格的drop-right函数，对越界情况容忍。
;;
;; 语法
;; ----
;; (list-drop-right lst n)
;;
;; 参数
;; ----
;; lst : list?
;; 源列表，从中删除元素。
;;
;; n : integer?
;; 要从末尾删除的元素数量。
;;
;; 返回值
;; ------
;; list
;; 返回去掉后n个元素后的列表。

(import (liii list) (liii check))

(check-set-mode! 'report-failed)

(check (list-drop-right '(1 2 3 4 5) 3) => '(1 2))
(check (list-drop-right '(1 2 3 4 5) 0) => '(1 2 3 4 5))
(check (list-drop-right '(1 2 3 4 5) 5) => '())

(check (list-drop-right '(1 2 3) -1) => '(1 2 3))
(check (list-drop-right '(1 2 3) 10) => '())

(check (list-drop-right '() 0) => '())

(check-catch 'type-error (list-drop-right "not a list" 2))
(check-catch 'type-error (list-drop-right '(1 2 3) "not a number"))

(check-report)
