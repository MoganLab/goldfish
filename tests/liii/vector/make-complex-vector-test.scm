(import (liii check) (liii vector))


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
;; 示例
;; ----
;; (make-complex-vector 3 0.0+1.0i) => #c(0.0+1.0i 0.0+1.0i 0.0+1.0i)
;; (make-complex-vector 0) => #c()
;;
;; 错误处理
;; ----
;; wrong-type-arg 当k不是合法整数或fill不是复数时


(check-true (complex-vector? (make-complex-vector 0)
            ) ;complex-vector?
) ;check-true
(check-true (complex-vector? (make-complex-vector 3)
            ) ;complex-vector?
) ;check-true
(check (make-complex-vector 0) => #())
(check (vector-length (make-complex-vector 5))
  =>
  5
) ;check


(let ((v (make-complex-vector 3)))
  (check (vector-length v) => 3)
  (check-true (complex-vector? v))
) ;let


(check-catch 'wrong-type-arg
  (make-complex-vector 'not-a-number)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-complex-vector 3 'not-a-complex)
) ;check-catch


(check-report)
