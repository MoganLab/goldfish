(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; set-find
;; 查找 set 中满足谓词的元素。
;;
;; 语法
;; ----
;; (set-find predicate set failure)
;;
;; 参数
;; ----
;; predicate : procedure
;; 一个接受一个参数并返回布尔值的函数。
;;
;; set : set
;; 要检查的 set。
;;
;; failure : procedure
;; 一个无参函数，当没有找到满足谓词的元素时调用。
;;
;; 返回值
;; ------
;; any
;; 如果找到满足 predicate 的元素，返回该元素；否则返回 failure 的调用结果。
;;
;; 注意
;; ----
;; 如果有多个元素满足谓词，返回其中任意一个。
;;
;; 示例
;; ----
;; (set-find (lambda (x) (= x 1)) (set 1 2) (lambda () 'not-found)) => 1
;; (set-find (lambda (x) (> x 10)) (set 1 2) (lambda () 'not-found)) => 'not-found
;;
;; 错误处理
;; ----
;; type-error
;; 当 set 参数不是 set 时抛出。


(define s-empty (set))
(define s-1 (set 1))
(define s-1-2 (set 1 2))
(define s-1-2-3 (set 1 2 3))


(check (set-find (lambda (x) (= x 1))
         s-1
         (lambda () 'not-found)
       ) ;set-find
  =>
  1
) ;check
(check (set-find (lambda (x) (= x 1))
         s-1-2
         (lambda () 'not-found)
       ) ;set-find
  =>
  1
) ;check
(check (set-find (lambda (x) (= x 2))
         s-1-2
         (lambda () 'not-found)
       ) ;set-find
  =>
  2
) ;check


(check (set-find (lambda (x) (> x 10))
         s-1
         (lambda () 'not-found)
       ) ;set-find
  =>
  'not-found
) ;check
(check (set-find (lambda (x) (> x 10))
         s-empty
         (lambda () 'not-found)
       ) ;set-find
  =>
  'not-found
) ;check


;; Test multiple elements satisfying predicate (returns any one)
(let ((res (set-find (lambda (x) (> x 0))
             s-1-2
             (lambda () 'not-found)
           ) ;set-find
      ) ;res
     ) ;
  (check-true (or (= res 1) (= res 2)))
) ;let


(check-catch 'type-error
  (set-find (lambda (x) #t)
    "not a set"
    (lambda () #f)
  ) ;set-find
) ;check-catch


(check-report)
