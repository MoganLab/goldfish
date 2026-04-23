(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector-set!
;; 修改向量指定位置的元素。
;;
;; 语法
;; ----
;; (vector-set! vector k obj)
;;
;; 参数
;; ----
;; vector : vector?
;; 待修改的向量。
;; k : exact-nonnegative-integer?
;; 元素的索引，从 0 开始。
;; obj : any
;; 新的元素值。
;;
;; 返回值
;; ------
;; 未指定（unspecified）。
;;
;; 说明
;; ----
;; 1. k 必须满足 0 <= k < (vector-length vector)
;; 2. 直接修改原向量
;; 3. 索引从 0 开始
(let ((v (vector 'a 'b 'c)))
  (vector-set! v 0 'x)
  (check v => #(x b c))
) ;let
(let ((v (vector 'a 'b 'c)))
  (vector-set! v 2 'z)
  (check v => #(a b z))
) ;let
(let ((v (vector 1 2 3)))
  (vector-set! v 1 99)
  (check v => #(1 99 3))
) ;let
(check-catch 'wrong-number-of-args
  (vector-set! #(1))
) ;check-catch
(check-catch 'wrong-number-of-args
  (vector-set! #(1) 0)
) ;check-catch
(check-catch 'wrong-number-of-args
  (vector-set! #(1) 0 0 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-set! '() 0 'a)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-set! #(1) 'a 'b)
) ;check-catch
(check-catch 'out-of-range
  (vector-set! #() 0 'a)
) ;check-catch
(check-catch 'out-of-range
  (vector-set! #(a) 1 'b)
) ;check-catch
(check-catch 'out-of-range
  (vector-set! #(a b) -1 'c)
) ;check-catch

(check-report)
