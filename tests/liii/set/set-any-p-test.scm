(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; set-any?
;; 检查 set 中是否有元素满足谓词。
;;
;; 语法
;; ----
;; (set-any? predicate set)
;;
;; 参数
;; ----
;; predicate : procedure
;; 一个接受一个参数并返回布尔值的函数。
;;
;; set : set
;; 要检查的 set。
;;
;; 返回值
;; ------
;; boolean
;; 如果 set 中至少有一个元素满足 predicate，返回 #t；否则返回 #f。
;;
;; 注意
;; ----
;; 与 SRFI 1 的 any 函数不同，此函数不返回满足谓词的元素，只返回布尔值。
;;
;; 示例
;; ----
;; (set-any? (lambda (x) (= x 1)) (set 1 2)) => #t
;; (set-any? (lambda (x) (> x 10)) (set 1 2)) => #f
;;
;; 错误处理
;; ----
;; type-error
;; 当 set 参数不是 set 时抛出。


(define s-empty (set))
(define s-1 (set 1))
(define s-1-2 (set 1 2))
(define s-1-2-3 (set 1 2 3))


(check-false (set-any? (lambda (x) (> x 10)) s-empty)
) ;check-false
(check-false (set-any? (lambda (x) (> x 10)) s-1)
) ;check-false
(check-false (set-any? (lambda (x) (> x 10)) s-1-2)
) ;check-false
(check-false (set-any? (lambda (x) (> x 10)) s-1-2-3)
) ;check-false


(check-true (set-any? (lambda (x) (= x 1)) s-1)
) ;check-true
(check-true (set-any? (lambda (x) (= x 1)) s-1-2)
) ;check-true
(check-true (set-any? (lambda (x) (= x 1)) s-1-2-3)
) ;check-true
(check-true (set-any? (lambda (x) (= x 2)) s-1-2)
) ;check-true
(check-true (set-any? (lambda (x) (= x 3)) s-1-2-3)
) ;check-true


;; Test multiple elements satisfying predicate
(check-true (set-any? (lambda (x) (> x 0)) s-1-2-3)
) ;check-true
(check-true (set-any? (lambda (x) (< x 10)) s-1-2-3)
) ;check-true


;; Test boundary cases
(check-true (set-any? (lambda (x) (even? x)) s-1-2)
) ;check-true
(check-false (set-any? (lambda (x) (even? x)) s-1)
) ;check-false
(check-true (set-any? (lambda (x) (odd? x)) s-1)
) ;check-true
(check-true (set-any? (lambda (x) (odd? x)) s-1-2)
) ;check-true


(check-catch 'type-error
  (set-any? (lambda (x) #t) "not a set")
) ;check-catch


(check-report)
