(import (liii check)
        (liii os)
        (liii vector))


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
;; 错误处理
;; ----
;; type-error 当vec不是complex-vector时
;; out-of-range 当索引越界时


(let ((v (complex-vector 1.0+2.0i 3.0+4.0i)))
  (if (os-windows?)
    (begin
      (check-true (number? (complex-vector-ref v 0)))
      (check-true (number? (complex-vector-ref v 1))))
    (begin
      (check (complex-vector-ref v 0) => 1.0+2.0i)
      (check (complex-vector-ref v 1) => 3.0+4.0i)))
) ;let

;; 多维 complex-vector 读取测试
(let ((m (make-complex-vector '(2 3) 5.0+6.0i)))
  (if (os-windows?)
    (begin
      (check-true (number? (complex-vector-ref m 0 0)))
      (check-true (number? (complex-vector-ref m 1 2))))
    (begin
      (check (complex-vector-ref m 0 0) => 5.0+6.0i)
      (check (complex-vector-ref m 1 2) => 5.0+6.0i)))
  (check-catch 'out-of-range (complex-vector-ref m -1 0))
  (check-catch 'out-of-range (complex-vector-ref m 2 0))
  (check-catch 'out-of-range (complex-vector-ref m 0 3))
  (check-catch 'type-error (complex-vector-ref m "0" 1))
  (check-catch 'type-error (complex-vector-ref m 0 "1"))
) ;let


(let ((v (make-complex-vector 3)))
  (check-true (number? (complex-vector-ref v 0)))
  (check-true (number? (complex-vector-ref v 1)))
  (check-true (number? (complex-vector-ref v 2)))
) ;let


(check-catch 'type-error (complex-vector-ref 'not-a-vector 0))
(check-catch 'type-error (complex-vector-ref (vector 1.0+2.0i 3.0+4.0i) 0))
(check-catch 'type-error (complex-vector-ref (int-vector 1 2) 0))
(check-catch 'out-of-range
  (complex-vector-ref (complex-vector 1.0+2.0i 3.0+4.0i) 5)
) ;check-catch
(check-catch 'out-of-range
  (complex-vector-ref (complex-vector 1.0+2.0i 3.0+4.0i) -1)
) ;check-catch


(check-report)
