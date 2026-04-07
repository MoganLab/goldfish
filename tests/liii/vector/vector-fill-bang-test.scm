(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; vector-fill!
;; 用指定值填充向量元素。
;;
;; 语法
;; ----
;; (vector-fill! vec fill)
;; (vector-fill! vec fill start)
;; (vector-fill! vec fill start end)
;;
;; 参数
;; ----
;; vec : vector?
;; 要填充的向量。
;;
;; fill : any?
;; 要填入的值。
;;
;; start : integer? 可选
;; 起始位置（包含），默认为0。
;;
;; end : integer? 可选
;; 结束位置（不包含），默认为向量长度。
;;
;; 返回值
;; ----
;; unspecified
;; 主要关注原向量被修改后的结果。
;;
;; 注意
;; ----
;; 这是原地修改操作，只会影响指定区间。
;;
;; 示例
;; ----
;; (vector-fill! #(1 2 3 4) 0) => 变为 #(0 0 0 0)
;; (vector-fill! #(1 2 3 4) #\x 1 3) => 变为 #(1 #\x #\x 4)
;;
;; 错误处理
;; ----
;; out-of-range 当start/end超出向量边界或start大于end时
;; wrong-type-arg 当vec不是向量，或start/end不是整数时

(let ((v (vector 1 2 3 4)))
  (vector-fill! v 0)
  (check v => #(0 0 0 0))
) ;let

(let ((v (vector 'a 'b 'c 'd)))
  (vector-fill! v 'x)
  (check v => #(x x x x))
) ;let

(let ((v (vector 1 2 3 4)))
  (vector-fill! v 'a 1)
  (check v => #(1 a a a))
) ;let

(let ((v (vector 1 2 3 4)))
  (vector-fill! v #\x 2)
  (check v => #(1 2 #\x #\x))
) ;let

(let ((v (vector 1 2 3 4)))
  (vector-fill! v #\x 1 3)
  (check v => #(1 #\x #\x 4))
) ;let

(let ((v (vector 1 2 3 4)))
  (vector-fill! v "hello" 0 2)
  (check v => #("hello" "hello" 3 4))
) ;let

(let ((v (vector)))
  (vector-fill! v 42)
  (check v => #())
) ;let

(let ((v (vector)))
  (vector-fill! v 42 0 0)
  (check v => #())
) ;let

(let ((v (vector 100)))
  (vector-fill! v 999)
  (check v => #(999))
) ;let

(let ((v (vector 100)))
  (vector-fill! v 999 0 1)
  (check v => #(999))
) ;let

(let ((v (vector 1 2 3 4 5)))
  (vector-fill! v "string")
  (check v => #("string" "string" "string" "string" "string"))
) ;let

(let ((v (vector 1 2 3 4 5)))
  (vector-fill! v 3.14 1 4)
  (check v => #(1 3.14 3.14 3.14 5))
) ;let

(let ((v (vector 1 2 3 4 5)))
  (vector-fill! v 'symbol 2 5)
  (check v => #(1 2 symbol symbol symbol))
) ;let

(let ((v (vector 1 2 3 4 5)))
  (vector-fill! v #\c 3 4)
  (check v => #(1 2 3 #\c 5))
) ;let

(let ((v (vector 1 2 3 4 5)))
  (vector-fill! v #t 0 1)
  (check v => #(#t 2 3 4 5))
) ;let

(let ((v (vector 0 1 2 3)))
  (vector-fill! v 99 0 0)
  (check v => #(0 1 2 3))
) ;let

(let ((v (vector 0 1 2 3)))
  (vector-fill! v 99 2 2)
  (check v => #(0 1 2 3))
) ;let

(let ((v (vector 0 1 2 3)))
  (vector-fill! v 99 0 1)
  (check v => #(99 1 2 3))
) ;let

(let ((v (vector 0 1 2 3)))
  (vector-fill! v 99 3 4)
  (check v => #(0 1 2 99))
) ;let

(check-catch 'wrong-type-arg (vector-fill! 'not-a-vector 42))
(check-catch 'wrong-type-arg (vector-fill! #(1 2 3) 42 'not-a-number))
(check-catch 'wrong-type-arg (vector-fill! #(1 2 3) 42 0 'not-a-number))

(let ((v #(1 2 3)))
  (check-catch 'out-of-range (vector-fill! v 42 -1))
  (check-catch 'out-of-range (vector-fill! v 42 4))
  (check-catch 'out-of-range (vector-fill! v 42 2 5))
  (check-catch 'out-of-range (vector-fill! v 42 3 2))
) ;let

(check-report)
