(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-unfold!
;; 将 `vector-unfold` 生成的元素写入已有向量的指定区间。
;;
;; 语法
;; ----
;; (vector-unfold! f vec start end initial-seed ...)
;;
;; 参数
;; ----
;; f : procedure
;;   (f k seed ...) 返回 n+1 个值：元素和新的 seeds。
;;   k 为从 0 开始的偏移索引。
;;
;; vec : vector
;;   被修改的目标向量。
;;
;; start : exact nonnegative integer
;;   写入起始索引（包含）。
;;
;; end : exact nonnegative integer
;;   写入结束索引（不包含）。
;;
;; initial-seed : any
;;   初始种子值。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 基本展开：填充整个向量
(let ((v (make-vector 5)))
  (vector-unfold! (lambda (i x) (values x (+ x 1))) v 0 5 10)
  (check v => #(10 11 12 13 14))
) ;let


;; 填充向量的一部分
(let ((v (make-vector 5 'x)))
  (vector-unfold! (lambda (i x) (values x (+ x 1))) v 1 4 100)
  (check v => #(x 100 101 102 x))
) ;let


;; 空区间（不修改）
(let ((v (make-vector 3 'a)))
  (vector-unfold! (lambda (i x) (values x x)) v 1 1 'ignored)
  (check v => #(a a a))
) ;let


;; 多种子展开
(let ((v (make-vector 6)))
  (vector-unfold! (lambda (i p q) (values p q (+ p q))) v 0 6 1 1)
  (check v => #(1 1 2 3 5 8))
) ;let


;; 从偏移 0 开始仅使用索引
(let ((v (make-vector 4)))
  (vector-unfold! values v 0 4)
  (check v => #(0 1 2 3))
) ;let


(check-report)
