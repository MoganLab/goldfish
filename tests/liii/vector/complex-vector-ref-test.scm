(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; complex-vector-ref
;; 返回复数向量中指定位置的元素。
;;
;; 语法
;; ----
;; (complex-vector-ref vec k)
;;
;; 参数
;; ----
;; vec : complex-vector?
;; 要访问的复数向量。
;;
;; k : integer?
;; 元素的索引（从0开始）。
;;
;; 返回值
;; ----
;; complex?
;; 向量中第k个位置的复数值。
;;
;; 注意
;; ----
;; 索引从0开始，超出范围会报错。
;;
;; 示例
;; ----
;; (complex-vector-ref (complex-vector 1+2i 3+4i 5+6i) 0) => 1+2i
;; (complex-vector-ref (complex-vector 1+2i 3+4i 5+6i) 2) => 5+6i
;;
;; 错误处理
;; ----
;; wrong-type-arg 当vec不是complex-vector时
;; out-of-range 当索引越界时


(let ((v (complex-vector 1.0+2.0i 3.0+4.0i)))
  (check-true (number? (complex-vector-ref v 0))
  ) ;check-true
  (check-true (number? (complex-vector-ref v 1))
  ) ;check-true
) ;let


(let ((v (make-complex-vector 3)))
  (check-true (number? (complex-vector-ref v 0))
  ) ;check-true
  (check-true (number? (complex-vector-ref v 1))
  ) ;check-true
  (check-true (number? (complex-vector-ref v 2))
  ) ;check-true
) ;let


(check-catch 'wrong-type-arg
  (complex-vector-ref 'not-a-vector 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (complex-vector-ref (vector 1.0+2.0i 3.0+4.0i)
    0
  ) ;complex-vector-ref
) ;check-catch
(check-catch 'wrong-type-arg
  (complex-vector-ref (int-vector 1 2) 0)
) ;check-catch
(check-catch 'out-of-range
  (complex-vector-ref (complex-vector 1.0+2.0i 3.0+4.0i)
    5
  ) ;complex-vector-ref
) ;check-catch
(check-catch 'out-of-range
  (complex-vector-ref (complex-vector 1.0+2.0i 3.0+4.0i)
    -1
  ) ;complex-vector-ref
) ;check-catch


(check-report)
