(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-unfold-right!
;; 将 `vector-unfold-right` 生成的元素以逆序写入已有向量的指定区间。
;;
;; 语法
;; ----
;; (vector-unfold-right! f vec start end initial-seed ...)
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
;; 基本展开：填充整个向量（逆序）
(let ((v (make-vector 5)))
  (vector-unfold-right! (lambda (i x) (values x (+ x 1))) v 0 5 10)
  (check v => #(14 13 12 11 10))
) ;let


;; 填充向量的一部分
(let ((v (make-vector 5 'x)))
  (vector-unfold-right! (lambda (i x) (values x (+ x 1))) v 1 4 100)
  (check v => #(x 102 101 100 x))
) ;let


;; 空区间（不修改）
(let ((v (make-vector 3 'a)))
  (vector-unfold-right! (lambda (i x) (values x x)) v 1 1 'ignored)
  (check v => #(a a a))
) ;let


;; 多种子展开
(let ((v (make-vector 6)))
  (vector-unfold-right! (lambda (i p q) (values p q (+ p q))) v 0 6 1 1)
  (check v => #(8 5 3 2 1 1))
) ;let


;; 从偏移 0 开始仅使用索引
(let ((v (make-vector 4)))
  (vector-unfold-right! values v 0 4)
  (check v => #(3 2 1 0))
) ;let


;; 与 vector-unfold! 对比
(let ((v1 (make-vector 5))
      (v2 (make-vector 5)))
  (vector-unfold! (lambda (i x) (values x (+ x 1))) v1 0 5 1)
  (vector-unfold-right! (lambda (i x) (values x (+ x 1))) v2 0 5 1)
  (check v1 => #(1 2 3 4 5))
  (check v2 => #(5 4 3 2 1))
) ;let


(check-report)
