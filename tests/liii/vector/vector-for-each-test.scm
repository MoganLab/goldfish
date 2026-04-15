(import (liii check)
  (liii list)
  (liii vector)
) ;import


(check-set-mode! 'report-failed)


;; vector-for-each
;; 对向量中的每个元素应用函数，主要用于副作用操作。
;;
;; 语法
;; ----
;; (vector-for-each proc vec1 vec2 ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 作用于每组元素的过程。
;;
;; vec1 vec2 ... : vector?
;; 一个或多个待遍历的向量。
;;
;; 返回值
;; ----
;; unspecified
;; 主要依赖副作用，不关注返回值。
;;
;; 注意
;; ----
;; 多个向量会按对应位置并行遍历。
;;
;; 示例
;; ----
;; (vector-for-each display #(1 2 3)) => 逐个输出元素
;;
;; 错误处理
;; ----
;; wrong-type-arg 当proc不是过程，或任一参数不是向量时


(check (let ((lst (make-list 5)))
         (vector-for-each (lambda (i) (list-set! lst i (* i i)))
           #(0 1 2 3 4)
         ) ;vector-for-each
         lst
       ) ;let
  =>
  '(0 1 4 9 16)
) ;check


(check (let ((lst (make-list 5)))
         (vector-for-each (lambda (i) (list-set! lst i (* i i)))
           #(0 1 2)
         ) ;vector-for-each
         lst
       ) ;let
  =>
  '(0 1 4 #f #f)
) ;check


(check (let ((lst (make-list 5)))
         (vector-for-each (lambda (i) (list-set! lst i (* i i)))
           #()
         ) ;vector-for-each
         lst
       ) ;let
  =>
  '(#f #f #f #f #f)
) ;check


(let ((sum 0))
  (vector-for-each (lambda (x) (set! sum (+ sum x)))
    #(1 2 3)
  ) ;vector-for-each
  (check sum => 6)
) ;let


(let ((result '()))
  (vector-for-each (lambda (x)
                     (set! result (cons x result))
                   ) ;lambda
    #(a b c)
  ) ;vector-for-each
  (check result => '(c b a))
) ;let


(let ((result '()))
  (vector-for-each (lambda (x y)
                     (set! result (cons (cons x y) result))
                   ) ;lambda
    #(a b c)
    #(1 2 3)
  ) ;vector-for-each
  (check result
    =>
    '((c . 3) (b . 2) (a . 1))
  ) ;check
) ;let


(let ((count 0))
  (vector-for-each (lambda (x) (set! count (+ count 1)))
    #()
  ) ;vector-for-each
  (check count => 0)
) ;let


(let ((value #f))
  (vector-for-each (lambda (x) (set! value x))
    #(42)
  ) ;vector-for-each
  (check value => 42)
) ;let


(check-catch 'wrong-type-arg
  (vector-for-each 'not-a-proc #(1 2 3))
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-for-each + 'not-a-vector)
) ;check-catch


(check-report)
