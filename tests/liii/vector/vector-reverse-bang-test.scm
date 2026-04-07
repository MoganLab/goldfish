(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; vector-reverse!
;; 原地反转向量，可选指定子区间。
;;
;; 语法
;; ----
;; (vector-reverse! vec)
;; (vector-reverse! vec start end)
;;
;; 参数
;; ----
;; vec : vector?
;; 要反转的向量。
;;
;; start : integer? 可选
;; 反转起始位置（包含），默认为0。
;;
;; end : integer? 可选
;; 反转结束位置（不包含），默认为向量长度。
;;
;; 返回值
;; ----
;; unspecified
;; 主要关注原向量被原地反转后的结果。
;;
;; 注意
;; ----
;; 只传vec时会反转整个向量；传start/end时只反转该子区间。
;;
;; 示例
;; ----
;; (vector-reverse! #(1 2 3 4)) => 变为 #(4 3 2 1)
;;
;; 错误处理
;; ----
;; wrong-number-of-args 当参数个数不正确时
;; type-error 当start/end类型不正确时
;; out-of-range 当start/end超出向量边界或start大于end时

(let ((vec (vector 1 2 3 4)))
  (vector-reverse! vec)
  (check vec => #(4 3 2 1))
) ;let

(let ((vec (vector 'a 'b 'c 'd)))
  (vector-reverse! vec 1 3)
  (check vec => #(a c b d))
) ;let

(let ((vec (vector 10 20 30)))
  (vector-reverse! vec 2 2)
  (check vec => #(10 20 30))
) ;let

(check-catch 'wrong-number-of-args (vector-reverse! (vector 1 2) 0 2 3))
(check-catch 'type-error (vector-reverse! (vector 1 2) 'a 2))
(check-catch 'type-error (vector-reverse! (vector 1 2) 0 'b))
(check-catch 'out-of-range (vector-reverse! (vector 1 2) -1 2))
(check-catch 'out-of-range (vector-reverse! (vector 1 2) 0 5))
(check-catch 'out-of-range (vector-reverse! (vector 1 2) 2 1))

(let ((vec (vector)))
  (vector-reverse! vec 0 0)
  (check vec => #())
) ;let

(let ((vec (vector 100)))
  (vector-reverse! vec)
  (check vec => #(100))
) ;let

(let ((vec (vector 1 2 3)))
  (vector-reverse! vec)
  (vector-reverse! vec)
  (check vec => #(1 2 3))
) ;let

(check-report)
