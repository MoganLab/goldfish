(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; complex-vector-set!
;; 设置复数向量中指定位置的元素。
;;
;; 语法
;; ----
;; (complex-vector-set! vec k value)
;;
;; 参数
;; ----
;; vec : complex-vector?
;; 要修改的复数向量。
;;
;; k : integer?
;; 元素的索引（从0开始）。
;;
;; value : complex?
;; 要设置的新值。
;;
;; 返回值
;; ----
;; 未指定（通常为void）。
;;
;; 注意
;; ----
;; 修改是破坏性的，会改变原向量。
;; 只能设置复数值。
;;
;; 示例
;; ----
;; (define v (complex-vector 1+2i 3+4i 5+6i))
;; (complex-vector-set! v 1 100+200i)
;; v => #c(1+2i 100+200i 5+6i)
;;
;; 错误处理
;; ----
;; wrong-type-arg 当vec不是complex-vector时
;; wrong-type-arg 当value不是复数时
;; out-of-range 当索引越界时

(let ((v (complex-vector 1+2i 3+4i)))
  (complex-vector-set! v 0 100+200i)
  (check-true (number? (complex-vector-ref v 0))))

(let ((v (make-complex-vector 5)))
  (complex-vector-set! v 2 3.14+2.71i)
  (check-true (number? (complex-vector-ref v 2))))

(let ((v (complex-vector 1.0+1.0i 2.0+2.0i)))
  (check-true (complex-vector? v)))

(let ((v (complex-vector 1+2i 3+4i)))
  (check-catch 'wrong-type-arg (complex-vector-set! v 0 'not-a-complex))
  (check-catch 'out-of-range (complex-vector-set! v 5 1+2i))
  (check-catch 'out-of-range (complex-vector-set! v -1 1+2i)))

(check-catch 'wrong-type-arg (complex-vector-set! (vector 1+2i 3+4i) 0 5+6i))
(check-catch 'wrong-type-arg (complex-vector-set! (int-vector 1 2) 0 3+4i))

(check-report)
