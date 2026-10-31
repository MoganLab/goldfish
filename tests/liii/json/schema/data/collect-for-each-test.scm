(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; collect-for-each
;; 遍历序列并收集每个元素经过处理后得到的列表。
;;
;; 语法
;; ----
;; (collect-for-each proc values)
;;
;; 参数
;; ----
;; proc : procedure
;; 处理过程，接收一个元素，返回一个列表。
;;
;; values : list
;; 待遍历的元素序列。
;;
;; 返回值
;; ----
;; list
;; 各元素处理结果的拼接，保持原有顺序。
;;
;; 注意
;; ----
;; 处理过程返回的列表会被拼接进结果，因此一个元素可以产出多个结果。
;;
;; 示例
;; ----
;; (collect-for-each (lambda (x) (list x (* x 2))) '(1 2)) => '(1 2 2 4)
;;
;; 错误处理
;; ----
;; 无。

(check
  (collect-for-each (lambda (x) (list x (* x 2))) '(1 2))
  =>
  '(1 2 2 4)
) ;check
(check (collect-for-each (lambda (x) (list x)) '()) => '())
(check (collect-for-each (lambda (x) '()) '(1 2)) => '())
(check (collect-for-each (lambda (x) (list (car x))) '((1 2) (3 4))) => '(1 3))

(check-report)
