(import (liii check)
        (liii error)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-fold
;; 对 set 中的每个元素应用 proc，累积结果并返回。
;;
;; 语法
;; ----
;; (set-fold proc nil set)
;;
;; 参数
;; ----
;; proc : procedure
;; 接收元素与累积值。
;;
;; nil : any
;; 初始累积值。
;;
;; set : set
;; 目标 set。
;;
;; 返回值
;; ------
;; any
;; 返回最后一次调用的结果，若 set 为空则返回 nil。
;;
;; 示例
;; ----
;; (set-fold (lambda (x acc) (+ x acc)) 0 (set 1 2 3)) => 6

(define s-empty (set))
(define s-1-2 (set 1 2))
(define s-1-2-3 (set 1 2 3))

;; Test sum
(check (set-fold (lambda (x acc) (+ x acc)) 0 s-1-2-3) => 6)
(check (set-fold (lambda (x acc) (+ x acc)) 0 s-empty) => 0)

;; Test accumulating to list (order not guaranteed)
(define fold-list (set-fold (lambda (x acc) (cons x acc)) '() s-1-2))
(check-true (set=? (list->set fold-list) s-1-2))

(check-catch 'type-error (set-fold (lambda (x acc) acc) '() "not a set"))

(check-report)
