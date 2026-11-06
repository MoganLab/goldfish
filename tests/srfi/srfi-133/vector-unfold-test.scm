(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-unfold
;; 基本向量构造器。创建长度为 length 的向量，在每个索引 k 上应用 f。
;;
;; 语法
;; ----
;; (vector-unfold f length initial-seed ...)
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
;; 基本展开：递减序列
(check (vector-unfold (lambda (i x) (values x (- x 1))) 10 0)
  =>
  #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
) ;check


;; 仅用索引构造序列
(check (vector-unfold values 5) => #(0 1 2 3 4))
(check (vector-unfold values 0) => #())
(check (vector-unfold values 1) => #(0))


;; 复制向量
(let ((v #(a b c d e)))
  (check (vector-unfold (lambda (i) (vector-ref v i)) (vector-length v)) => v)
) ;let


;; 多种子展开：斐波那契数列
(check (vector-unfold (lambda (i p q) (values p q (+ p q))) 10 1 1)
  =>
  #(1 1 2 3 5 8 13 21 34 55)
) ;check


;; 空向量
(check (vector-unfold (lambda (i x) (values x x)) 0 'ignored) => #())


(check-report)
