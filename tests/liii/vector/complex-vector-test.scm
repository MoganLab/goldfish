(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; complex-vector
;; 创建只包含复数元素的专用向量。
;;
;; 语法
;; ----
;; (complex-vector obj ...)
;;
;; 参数
;; ----
;; obj : complex?
;; 要放入向量的复数元素。
;;
;; 返回值
;; ----
;; complex-vector
;; 一个新的复数向量。
;;
;; 注意
;; ----
;; complex-vector 会在构造时检查所有参数是否为复数。
;;
;; 示例
;; ----
;; (complex-vector 1+2i 3+4i) => 一个包含复数的向量
;;
;; 错误处理
;; ----
;; wrong-type-arg 当任一参数不是复数时

(check-true (vector? (complex-vector 1+2i 3+4i)))
(check-true (complex-vector? (complex-vector 1+2i 3+4i)))
(check-catch 'wrong-type-arg (complex-vector 1+2i 'a))

(let ((v (complex-vector)))
  (check (vector-length v) => 0)
  (check-true (complex-vector? v))
) ;let

(let ((v (complex-vector 1+2i)))
  (check (vector-length v) => 1)
  (check-true (complex-vector? v))
) ;let

(check-report)
