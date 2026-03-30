(import (liii check)
        (liii error)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-for-each
;; 对 set 中的每个元素应用 proc，忽略返回值。
;;
;; 语法
;; ----
;; (set-for-each proc set)
;;
;; 参数
;; ----
;; proc : procedure
;; 要应用的函数。
;;
;; set : set
;; 目标 set。
;;
;; 返回值
;; ------
;; unspecified
;; 返回值未指定。
;;
;; 示例
;; ----
;; (set-for-each (lambda (x) (display x)) (set 1 2 3))

(define s-empty (set))

;; Test basic behavior
(define s-foreach (set 1 2 3))
(define foreach-collected '())
(set-for-each (lambda (x) (set! foreach-collected (cons x foreach-collected))) s-foreach)
(check-true (set=? (list->set foreach-collected) s-foreach))

;; Test empty set - no calls triggered
(define foreach-count 0)
(set-for-each (lambda (x) (set! foreach-count (+ foreach-count 1))) s-empty)
(check (set-size s-empty) => 0)
(check foreach-count => 0)

(check-catch 'type-error (set-for-each (lambda (x) x) "not a set"))

(check-report)
