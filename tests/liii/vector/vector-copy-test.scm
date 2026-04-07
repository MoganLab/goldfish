(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; vector-copy
;; 创建向量副本，可选复制子区间。
;;
;; 语法
;; ----
;; (vector-copy vec)
;; (vector-copy vec start)
;; (vector-copy vec start end)
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
;; 一个新的向量副本。
;;
;; 注意
;; ----
;; 返回的新向量与原向量内容相同，但不是同一个对象。
;;
;; 示例
;; ----
;; (vector-copy #(0 1 2 3)) => #(0 1 2 3)
;; (vector-copy #(0 1 2 3) 1 3) => #(1 2)
;;
;; 错误处理
;; ----
;; out-of-range 当start/end超出向量边界或start大于end时
;; wrong-type-arg 当vec不是向量，或start/end不是整数时

(check (vector-copy #(0 1 2 3)) => #(0 1 2 3))
(check (vector-copy #(0 1 2 3) 1) => #(1 2 3))
(check (vector-copy #(0 1 2 3) 3) => #(3))
(check (vector-copy #(0 1 2 3) 4) => #())

(check-catch 'out-of-range (vector-copy #(0 1 2 3) 5))
(check-catch 'out-of-range (vector-copy #(0 1 2 3) 1 5))

(define my-vector #(0 1 2 3))
(check (eqv? my-vector (vector-copy #(0 1 2 3))) => #f)
(check-true
  (eqv? (vector-ref my-vector 2)
        (vector-ref (vector-copy #(0 1 2 3)) 2)
  ) ;eqv?
) ;check-true

(check (vector-copy #(0 1 2 3) 1 1) => #())
(check (vector-copy #(0 1 2 3) 1 2) => #(1))
(check (vector-copy #(0 1 2 3) 1 4) => #(1 2 3))
(check (vector-copy #()) => #())
(check (vector-copy #() 0) => #())
(check (vector-copy #() 0 0) => #())
(check (vector-copy #(42)) => #(42))
(check (vector-copy #(42) 0) => #(42))
(check (vector-copy #(42) 0 1) => #(42))
(check (vector-copy #(42) 1) => #())

(let ((v #(1 2.5 "hello" 'symbol #\c #t #f)))
  (check (vector-copy v) => v)
  (check (vector-copy v 2 5) => #("hello" 'symbol #\c))
) ;let

(check (vector-copy #((1 2) (3 4))) => #((1 2) (3 4)))
(check (vector-copy #((1 2) (3 4)) 1) => #((3 4)))

(let ((original #(a b c)))
  (let ((copied (vector-copy original)))
    (check-true (vector? copied))
    (check-false (eq? original copied))
    (check-true (eqv? (vector-ref original 1) (vector-ref copied 1)))
    (check (vector-length copied) => (vector-length original))
  ) ;let
) ;let

(let ((original #(1 2 3)))
  (let ((copied (vector-copy original)))
    (vector-set! copied 1 99)
    (check original => #(1 2 3))
    (check copied => #(1 99 3))
  ) ;let
) ;let

(check (vector-copy #(0 1 2 3) 0 0) => #())
(check (vector-copy #(0 1 2 3) 2 2) => #())
(check (vector-copy #(0 1 2 3) 0 1) => #(0))
(check (vector-copy #(0 1 2 3) 3 4) => #(3))

(check-catch 'wrong-type-arg (vector-copy 'not-a-vector))
(check-catch 'wrong-type-arg (vector-copy #(1 2 3) 'not-a-number))
(check-catch 'wrong-type-arg (vector-copy #(1 2 3) 0 'not-a-number))

(let ((v #(1 2 3)))
  (check-catch 'out-of-range (vector-copy v -1))
  (check-catch 'out-of-range (vector-copy v 4))
  (check-catch 'out-of-range (vector-copy v 2 5))
  (check-catch 'out-of-range (vector-copy v 3 2))
) ;let

(check-report)
