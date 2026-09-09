(import (liii check)
        (liii os)
        (liii vector))


(check-set-mode! 'report-failed)


;; make-complex-vector
;; 创建指定长度的复数向量。
;;
;; 语法
;; ----
;; (make-complex-vector k)
;; (make-complex-vector k fill)
;;
;; 参数
;; ----
;; k : integer?
;; 目标向量长度，必须为非负整数。
;;
;; fill : complex? 可选
;; 初始化每个元素时使用的复数值，默认为0.0+0.0i。
;;
;; 返回值
;; ----
;; complex-vector
;; 一个长度为k的新复数向量。
;;
;; 注意
;; ----
;; 创建的向量是complex-vector类型，只能存储复数。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当k不是合法整数或fill不是复数时


(check-true (complex-vector? (make-complex-vector 0)))
(check-true (complex-vector? (make-complex-vector 3)))
(check (make-complex-vector 0) => #())
(check (vector-length (make-complex-vector 5)) => 5)


(let ((v (make-complex-vector 3)))
  (check (vector-length v) => 3)
  (check-true (complex-vector? v))
) ;let

(let ((v (make-complex-vector 3 1.0+2.0i)))
  (if (os-windows?)
    (begin
      (check-true (number? (complex-vector-ref v 0)))
      (check-true (number? (complex-vector-ref v 2))))
    (begin
      (check (complex-vector-ref v 0) => 1.0+2.0i)
      (check (complex-vector-ref v 2) => 1.0+2.0i)))
) ;let

(let ((mv (make-complex-vector '(2 3) 2.0+3.0i)))
  (check-true (complex-vector? mv))
  (check (vector-dimensions mv) => '(2 3))
  (if (os-windows?)
    (check-true (number? (complex-vector-ref mv 1 2)))
    (check (complex-vector-ref mv 1 2) => 2.0+3.0i))
) ;let


(check-catch 'wrong-type-arg (make-complex-vector 'not-a-number))
(check-catch 'wrong-type-arg (make-complex-vector 3 'not-a-complex))
(check-catch 'out-of-range (make-complex-vector -1))
(check-catch 'out-of-range (make-complex-vector -1 1.0+1.0i))
(check-catch 'wrong-number-of-args (make-complex-vector))
(check-catch 'wrong-number-of-args (make-complex-vector 1 2 3))


(check-report)
