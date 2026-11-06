(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-reverse-copy
;; 创建向量的反向副本，可选指定子区间。
;;
;; 语法
;; ----
;; (vector-reverse-copy vec)
;; (vector-reverse-copy vec start)
;; (vector-reverse-copy vec start end)
;;
;; 参数
;; ----
;; vec : vector?
;; 要复制的向量。
;;
;; start : integer? 可选
;; 复制起始位置（包含），默认为0。
;;
;; end : integer? 可选
;; 复制结束位置（不包含），默认为向量长度。
;;
;; 返回值
;; ----
;; vector
;; 一个新的反向向量副本。
;;
;; 注意
;; ----
;; 原向量不会被修改。


(let ((vec #(1 2 3)))
  (let ((rev (vector-reverse-copy vec)))
    (check rev => #(3 2 1))
    (check vec => #(1 2 3))
  ) ;let
) ;let


(check (vector-reverse-copy #(1 2 3 4)) => #(4 3 2 1))
(check (vector-reverse-copy #(1 2 3 4) 1) => #(4 3 2))
(check (vector-reverse-copy #(1 2 3 4) 1 3) => #(3 2))


(check (vector-reverse-copy #()) => #())
(check (vector-reverse-copy #(1) 0 0) => #())
(check (vector-reverse-copy #(1 2) 1 1) => #())


(let ((original #(a b c)))
  (let ((copied (vector-reverse-copy original)))
    (check-true (vector? copied))
    (check-false (eq? original copied))
    (check copied => #(c b a))
  ) ;let
) ;let


(let ((original #(1 2 3)))
  (let ((copied (vector-reverse-copy original)))
    (vector-set! copied 1 99)
    (check original => #(1 2 3))
    (check copied => #(3 99 1))
  ) ;let
) ;let


(check-report)
