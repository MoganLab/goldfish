(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; set-count
;; 计算 set 中满足谓词的元素个数。
;;
;; 语法
;; ----
;; (set-count predicate set)
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
;; exact-integer
;; 返回满足 predicate 的元素个数（精确整数）。
;;
;; 示例
;; ----
;; (set-count (lambda (x) (> x 0)) (set 1 2 3)) => 3
;; (set-count (lambda (x) (> x 1)) (set 1 2 3)) => 2
;;
;; 错误处理
;; ----
;; type-error
;; 当 set 参数不是 set 时抛出。


(define s-empty (set))
(define s-1 (set 1))
(define s-1-2 (set 1 2))
(define s-1-2-3 (set 1 2 3))


(check (set-count (lambda (x) (> x 0)) s-empty)
  =>
  0
) ;check
(check (set-count (lambda (x) (> x 0)) s-1)
  =>
  1
) ;check
(check (set-count (lambda (x) (> x 0)) s-1-2)
  =>
  2
) ;check
(check (set-count (lambda (x) (> x 0)) s-1-2-3)
  =>
  3
) ;check


(check (set-count (lambda (x) (> x 1)) s-1)
  =>
  0
) ;check
(check (set-count (lambda (x) (> x 1)) s-1-2)
  =>
  1
) ;check
(check (set-count (lambda (x) (> x 1)) s-1-2-3)
  =>
  2
) ;check


(check (set-count (lambda (x) (even? x))
         s-1-2-3
       ) ;set-count
  =>
  1
) ;check
(check (set-count (lambda (x) (odd? x))
         s-1-2-3
       ) ;set-count
  =>
  2
) ;check


(check-catch 'type-error
  (set-count (lambda (x) #t) "not a set")
) ;check-catch


(check-report)
