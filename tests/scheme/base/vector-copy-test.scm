(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector-copy
;; 复制向量或其中一部分。
;;
;; 语法
;; ----
;; (vector-copy vector [start [end]])
;;
;; 参数
;; ----
;; vector : vector?
;; 待复制的向量。
;; start : exact-nonnegative-integer?
;; 可选，起始索引，默认为 0。
;; end : exact-nonnegative-integer?
;; 可选，结束索引，默认为向量长度。
;;
;; 返回值
;; ------
;; vector?
;; 新向量，包含指定范围内的元素。
;;
;; 说明
;; ----
;; 1. 返回新向量，不修改原向量
;; 2. 复制范围为 [start, end)
(check (vector-copy #(a b c))
  =>
  #(a b c)
) ;check
(check (vector-copy #(a b c) 0)
  =>
  #(a b c)
) ;check
(check (vector-copy #(a b c) 1)
  =>
  #(b c)
) ;check
(check (vector-copy #(a b c) 1 3)
  =>
  #(b c)
) ;check
(check (vector-copy #(a b c) 0 0)
  =>
  #()
) ;check
(check (vector-copy #(1 2 3 4 5) 2 4)
  =>
  #(3 4)
) ;check
(let ((v #(a b c)))
  (let ((copy (vector-copy v)))
    (vector-set! copy 0 'x)
    (check v => #(a b c))
    (check copy => #(x b c))
  ) ;let
) ;let
(check-catch 'wrong-number-of-args
  (vector-copy)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-copy '())
) ;check-catch
(check-catch 'out-of-range
  (vector-copy #(a) 2)
) ;check-catch
(check-catch 'out-of-range
  (vector-copy #(a) -1)
) ;check-catch

(check-report)
