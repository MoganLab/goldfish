(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector-fill!
;; 将向量的所有元素替换为指定值。
;;
;; 语法
;; ----
;; (vector-fill! vector fill [start [end]])
;;
;; 参数
;; ----
;; vector : vector?
;; 待修改的向量。
;; fill : any
;; 用于填充的值。
;; start : exact-nonnegative-integer?
;; 可选，起始索引，默认为 0。
;; end : exact-nonnegative-integer?
;; 可选，结束索引，默认为向量长度。
;;
;; 返回值
;; ------
;; 未指定（unspecified）。
;;
;; 说明
;; ----
;; 1. 直接修改原向量
;; 2. 空向量调用无效果
(let ((v (vector 'a 'b 'c)))
  (vector-fill! v 'x)
  (check v => #(x x x))
) ;let
(let ((v (vector 1 2 3 4 5)))
  (vector-fill! v 0 1 4)
  (check v => #(1 0 0 0 5))
) ;let
(let ((v (vector)))
  (vector-fill! v 'x)
  (check v => #())
) ;let
(let ((v (vector 'a 'b)))
  (vector-fill! v 'y 0 2)
  (check v => #(y y))
) ;let
(check-catch 'wrong-number-of-args
  (vector-fill! #(1))
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-fill! "abc" 'x)
) ;check-catch
(check-catch 'out-of-range
  (vector-fill! #(a) 'x 2)
) ;check-catch
(check-catch 'out-of-range
  (vector-fill! #(a) 'x -1)
) ;check-catch

(check-report)
