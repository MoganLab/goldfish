;; remove
;; 从列表中删除满足谓词条件的元素。
;;
;; 语法
;; ----
;; (remove pred list)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受单个参数并返回布尔值。
;;
;; list : list?
;; 要处理的列表。
;;
;; 返回值
;; ------
;; list
;; 返回不满足谓词条件的元素组成的新列表。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (remove even? '(-2 -1 0 1 2)) => '(-1 1))

(check-catch 'type-error (remove 1 '(1 2 3)))
(check-catch 'type-error (remove even? 1))

(check-report)
