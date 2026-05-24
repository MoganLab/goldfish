(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-reverse-copy!
;; 将源向量的指定区间反向复制到目标向量的指定位置。
;;
;; 语法
;; ----
;; (vector-reverse-copy! to at from)
;; (vector-reverse-copy! to at from start)
;; (vector-reverse-copy! to at from start end)
;;
;; 参数
;; ----
;; to : vector?
;; 目标向量。
;;
;; at : integer?
;; 复制到目标向量的起始位置。
;;
;; from : vector?
;; 源向量。
;;
;; start : integer? 可选
;; 源向量起始位置（包含），默认为0。
;;
;; end : integer? 可选
;; 源向量结束位置（不包含），默认为源向量长度。
;;
;; 返回值
;; ----
;; unspecified
;; 主要关注目标向量被复制后的结果。
;;
;; 注意
;; ----
;; 源向量的区间 [start, end) 被反向后写入目标向量的 [at, at + end - start)。
;; 允许源和目标为同一向量，但重叠区间行为由底层 vector-copy! 决定。


(let ((to (vector 1 2 3 4 5)) (from (vector 20 30 40)))
  (vector-reverse-copy! to 1 from)
  (check to => #(1 40 30 20 5))
) ;let


(let ((to (vector 1 2 3 4 5)) (from (vector 10 20 30 40 50)))
  (vector-reverse-copy! to 0 from 1 4)
  (check to => #(40 30 20 4 5))
) ;let


(let ((to (vector 1 2 3 4 5)))
  (vector-reverse-copy! to 0 to 0 5)
  (check to => #(5 4 3 2 1))
) ;let


(let ((to (vector 'a 'b 'c 'd 'e)))
  (vector-reverse-copy! to 1 to 0 3)
  (check to => #(a c b a e))
) ;let


(let ((to (vector 1 2 3)) (from (vector)))
  (vector-reverse-copy! to 0 from)
  (check to => #(1 2 3))
) ;let


(let ((to (vector 1 2 3 4)))
  (vector-reverse-copy! to 2 to 0 2)
  (check to => #(1 2 2 1))
) ;let


(check-report)
