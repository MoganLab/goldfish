(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; float-vector
;; 创建只包含浮点数元素的专用向量。
;;
;; 语法
;; ----
;; (float-vector obj ...)
;;
;; 参数
;; ----
;; obj : real?
;; 要放入向量的浮点数元素。
;;
;; 返回值
;; ----
;; float-vector
;; 一个新的浮点数向量。
;;
;; 注意
;; ----
;; float-vector 会在构造时将整数转换为浮点数。
;;
;; 示例
;; ----
;; (float-vector 1.0 2.0 3.0) => #r(1.0 2.0 3.0)
;; (float-vector 1 2 3) => #r(1.0 2.0 3.0)
;;
;; 错误处理
;; ----
;; wrong-type-arg 当任一参数不是数字时


(check-true (vector? (float-vector 1.0 2.0 3.0))
) ;check-true
(check-true (float-vector? (float-vector 1.0 2.0 3.0)
            ) ;float-vector?
) ;check-true
(check (float-vector 1.0 2.0 3.0)
  =>
  #(1.0 2.0 3.0)
) ;check
(check (float-vector 1 2 3)
  =>
  #(1.0 2.0 3.0)
) ;check
(check-catch 'wrong-type-arg
  (float-vector 1.0 2.0 'a)
) ;check-catch


(let ((v (float-vector)))
  (check (vector-length v) => 0)
  (check-true (float-vector? v))
) ;let


(let ((v (float-vector 3.14)))
  (check (vector-length v) => 1)
  (check-true (float-vector? v))
  (check (float-vector-ref v 0) => 3.14)
) ;let


(check-report)
