(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector-for-each
;; 对向量中的每个元素依次应用过程。
;;
;; 语法
;; ----
;; (vector-for-each proc vector1 vector2 ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接受元素参数的过程。
;; vector1, vector2, ... : vector?
;; 至少一个向量。
;;
;; 返回值
;; ------
;; 未指定（unspecified）。
;;
;; 说明
;; ----
;; 1. 按索引顺序依次调用 proc
;; 2. 多个向量时，proc 接收对应位置的元素
;; 3. 遍历到最短向量的长度为止
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
                     (set! result (cons (list x y) result))
                   ) ;lambda
    #(1 2)
    #(10 20)
  ) ;vector-for-each
  (check result => '((2 20) (1 10)))
) ;let
(let ((result '()))
  (vector-for-each (lambda (x)
                     (set! result (cons x result))
                   ) ;lambda
    #()
  ) ;vector-for-each
  (check result => '())
) ;let
(check-catch 'wrong-number-of-args
  (vector-for-each)
) ;check-catch
(check-catch 'wrong-number-of-args
  (vector-for-each (lambda (x) x))
) ;check-catch

(check-report)
