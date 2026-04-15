(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; set-every?
;; 检查 set 中是否所有元素都满足谓词。
;;
;; 语法
;; ----
;; (set-every? predicate set)
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
;; 如果 set 中所有元素都满足 predicate，返回 #t；否则返回 #f。
;;
;; 注意
;; ----
;; 与 SRFI 1 的 every 函数不同，此函数不返回满足谓词的元素，只返回布尔值。
;; 空 set 返回 #t。
;;
;; 示例
;; ----
;; (set-every? (lambda (x) (> x 0)) (set 1 2)) => #t
;; (set-every? (lambda (x) (> x 1)) (set 1 2)) => #f
;;
;; 错误处理
;; ----
;; type-error
;; 当 set 参数不是 set 时抛出。


(define s-empty (set))
(define s-1 (set 1))
(define s-1-2 (set 1 2))
(define s-1-2-3 (set 1 2 3))


(check-true (set-every? (lambda (x) (> x 0))
              s-empty
            ) ;set-every?
) ;check-true
(check-true (set-every? (lambda (x) (> x 0)) s-1)
) ;check-true
(check-true (set-every? (lambda (x) (> x 0)) s-1-2)
) ;check-true
(check-true (set-every? (lambda (x) (> x 0))
              s-1-2-3
            ) ;set-every?
) ;check-true


(check-false (set-every? (lambda (x) (> x 1)) s-1)
) ;check-false
(check-false (set-every? (lambda (x) (> x 1)) s-1-2)
) ;check-false
(check-false (set-every? (lambda (x) (> x 1))
               s-1-2-3
             ) ;set-every?
) ;check-false


(check-true (set-every? (lambda (x) (number? x))
              s-1-2-3
            ) ;set-every?
) ;check-true


;; Test boundary cases
(check-true (set-every? (lambda (x) (odd? x)) s-1)
) ;check-true
(check-false (set-every? (lambda (x) (odd? x)) s-1-2)
) ;check-false
(check-false (set-every? (lambda (x) (even? x))
               s-1-2
             ) ;set-every?
) ;check-false


(check-catch 'type-error
  (set-every? (lambda (x) #t) "not a set")
) ;check-catch


(check-report)
