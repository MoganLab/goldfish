(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector-append
;; 将多个向量连接为一个新向量。
;;
;; 语法
;; ----
;; (vector-append vector ...)
;;
;; 参数
;; ----
;; vector ... : vector?
;; 任意数量的向量。
;;
;; 返回值
;; ------
;; vector?
;; 所有输入向量按顺序连接后的新向量。
;;
;; 说明
;; ----
;; 1. 无参数时返回空向量
;; 2. 单参数时返回该参数的副本
;; 3. 返回新向量，不修改原向量
(check (vector-append) => #())
(check (vector-append #(a)) => #(a))
(check (vector-append #(a b) #(c d))
  =>
  #(a b c d)
) ;check
(check (vector-append #(1) #(2) #(3))
  =>
  #(1 2 3)
) ;check
(check (vector-append #() #(a)) => #(a))
(check (vector-append #(a) #()) => #(a))
(check (vector-append #() #()) => #())
(let ((v #(a b)))
  (let ((result (vector-append v #(c))))
    (vector-set! result 0 'x)
    (check v => #(a b))
    (check result => #(x b c))
  ) ;let
) ;let
(check-catch 'wrong-type-arg
  (vector-append 'a)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-append #(1) 'a)
) ;check-catch

(check-report)
