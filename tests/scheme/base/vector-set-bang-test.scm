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

;; 二维向量特化更新（覆盖 g_vector_set_4）
(let ((m (make-vector '(2 3) 0)))
  (vector-set! m 0 1 10)
  (vector-set! m 1 2 20)
  (check (vector-ref m 0 1) => 10)
  (check (vector-ref m 1 2) => 20)
  (check-catch 'out-of-range (vector-set! m -1 0 1))
  (check-catch 'out-of-range (vector-set! m 2 0 1))
  (check-catch 'out-of-range (vector-set! m 0 3 1))
  (check-catch 'wrong-type-arg (vector-set! m "0" 1 2))
  (check-catch 'wrong-type-arg (vector-set! m 0 "1" 2))
) ;let

;; 三维向量多维更新（覆盖 g_vector_set 多维分支）
(let ((m (make-vector '(2 2 2) 0)))
  (vector-set! m 1 0 1 99)
  (check (vector-ref m 1 0 1) => 99)
  (check-catch 'wrong-number-of-args (vector-set! m 1 0 1 2 3))
) ;let

;; 不可变向量检查
(let ((v (immutable! (vector 1 2 3))))
  (check-catch 'immutable-error (vector-set! v 0 10))
) ;let

(check-catch 'wrong-number-of-args (vector-set! #(1)))
(check-catch 'wrong-number-of-args (vector-set! #(1) 0))
(check-catch 'wrong-number-of-args (vector-set! #(1) 0 0 0))
(check-catch 'wrong-type-arg (vector-set! '() 0 'a))
(check-catch 'wrong-type-arg (vector-set! #(1) 'a 'b))
(check-catch 'out-of-range (vector-set! #() 0 'a))
(check-catch 'out-of-range (vector-set! #(a) 1 'b))
(check-catch 'out-of-range (vector-set! #(a b) -1 'c))

(check-report)
