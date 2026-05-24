(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-unfold-right
;; 从右向左展开的向量构造器。
;;
;; 语法
;; ----
;; (vector-unfold-right f length initial-seed ...)
;;
;; 参数
;; ----
;; f : procedure
;;   (f k seed ...) 返回 n+1 个值：元素和新的 seeds。
;;
;; length : exact nonnegative integer
;;   要构造的向量长度。
;;
;; initial-seed : any
;;   初始种子值。
;;
;; 返回值
;; ----
;; vector
;; 新构造的向量。
;;
;; 基本展开：索引与种子配对
(check (vector-unfold-right (lambda (i x) (values (cons i x) (+ x 1))) 5 0)
  =>
  #((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
) ;check


;; 反转向量
(let ((v #(a b c d e)))
  (check (vector-unfold-right (lambda (i x) (values (vector-ref v x) (+ x 1)))
           (vector-length v)
           0
         ) ;vector-unfold-right
    =>
    #(e d c b a)
  ) ;check
) ;let


;; 仅用索引构造序列（从右到左，结果与 unfold 相同）
(check (vector-unfold-right values 5) => #(0 1 2 3 4))
(check (vector-unfold-right values 0) => #())
(check (vector-unfold-right values 1) => #(0))


;; 多种子展开
(check (vector-unfold-right (lambda (i p q) (values p q (+ p q))) 10 1 1)
  =>
  #(55 34 21 13 8 5 3 2 1 1)
) ;check


;; 空向量
(check (vector-unfold-right (lambda (i x) (values x x)) 0 'ignored) => #())


;; vector-unfold 与 vector-unfold-right 结果对比
(let ((unfold-result (vector-unfold (lambda (i x) (values x (+ x 1))) 5 1))
      (unfold-right-result (vector-unfold-right (lambda (i x) (values x (+ x 1))) 5 1)
      ) ;unfold-right-result
     ) ;
  (check unfold-result => #(1 2 3 4 5))
  (check unfold-right-result => #(5 4 3 2 1))
) ;let


(check-report)
