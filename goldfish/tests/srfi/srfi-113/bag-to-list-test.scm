;; bag->list
;; 将 bag 转换为列表，重复元素按出现次数保留。
;;
;; 语法
;; ----
;; (bag->list bag)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; 返回值
;; ----
;; 返回包含 bag 全部元素的列表，重复元素按出现次数保留。

(import (liii check) (liii bag) (liii error))

(check-set-mode! 'report-failed)

;; Data Setup

(define b-empty (bag))

(define b-1-2 (bag 1 2 2))


;; 统计列表中元素 x 的出现次数

(define (count-occurrences lst x)
  (let loop
    ((rem lst) (n 0))
    (if (null? rem)
      n
      (loop (cdr rem) (if (equal? (car rem) x) (+ n 1) n))
    )
  ) ;let
) ;define


;; 空 bag 转换为空列表

(check (bag->list b-empty) => '())


;; 重复元素按出现次数保留

(define l-1-2 (bag->list b-1-2))

(check (length l-1-2) => 3)
(check (count-occurrences l-1-2 1) => 1)
(check (count-occurrences l-1-2 2) => 2)
(check (count-occurrences l-1-2 3) => 0)

(check-catch 'type-error (bag->list "not a bag"))

(check-report)
