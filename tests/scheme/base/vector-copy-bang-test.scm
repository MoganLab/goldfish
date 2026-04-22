(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector-copy!
;; 将源向量的元素复制到目标向量的指定位置。
;;
;; 语法
;; ----
;; (vector-copy! to at from [start [end]])
;;
;; 参数
;; ----
;; to : vector?
;; 目标向量。
;; at : exact-nonnegative-integer?
;; 目标向量的起始位置。
;; from : vector?
;; 源向量。
;; start : exact-nonnegative-integer?
;; 可选，源向量的起始索引，默认为 0。
;; end : exact-nonnegative-integer?
;; 可选，源向量的结束索引，默认为源向量长度。
;;
;; 返回值
;; ------
;; 未指定（unspecified）。
;;
;; 说明
;; ----
;; 1. 直接修改目标向量
;; 2. 源向量和目标向量可以是同一个向量
(let ((to (vector 'a 'b 'c 'd 'e)))
  (vector-copy! to 1 #(x y))
  (check to => #(a x y d e))
) ;let
(let ((to (vector 'a 'b 'c)))
  (vector-copy! to 0 #(1 2 3))
  (check to => #(1 2 3))
) ;let
(let ((to (vector 'a 'b 'c 'd)))
  (vector-copy! to 2 #(x y) 0 2)
  (check to => #(a b x y))
) ;let
(let ((v (vector 'a 'b 'c)))
  (vector-copy! v 0 v 1)
  (check v => #(b c c))
) ;let
(check-catch 'wrong-number-of-args
  (vector-copy! #(1) 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-copy! '() 0 #(1))
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-copy! #(1) 'a #(1))
) ;check-catch
(check-catch 'out-of-range
  (vector-copy! #(a) 1 #(b))
) ;check-catch
(check-catch 'out-of-range
  (vector-copy! #(a) -1 #(b))
) ;check-catch

(check-report)
