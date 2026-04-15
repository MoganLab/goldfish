(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; make-float-vector
;; 创建指定长度的浮点数向量。
;;
;; 语法
;; ----
;; (make-float-vector k)
;; (make-float-vector k fill)
;;
;; 参数
;; ----
;; k : integer?
;; 目标向量长度，必须为非负整数。
;;
;; fill : real? 可选
;; 初始化每个元素时使用的浮点数值，默认为0.0。
;;
;; 返回值
;; ----
;; float-vector
;; 一个长度为k的新浮点数向量。
;;
;; 注意
;; ----
;; 创建的向量是float-vector类型，只能存储浮点数。
;;
;; 示例
;; ----
;; (make-float-vector 3 1.0) => #r(1.0 1.0 1.0)
;; (make-float-vector 0) => #r()
;;
;; 错误处理
;; ----
;; wrong-type-arg 当k不是合法整数或fill不是实数时


(check-true (float-vector? (make-float-vector 0))
) ;check-true
(check-true (float-vector? (make-float-vector 3))
) ;check-true
(check (make-float-vector 3 0.0)
  =>
  #(0.0 0.0 0.0)
) ;check
(check (make-float-vector 3 1.0)
  =>
  #(1.0 1.0 1.0)
) ;check
(check (make-float-vector 0) => #())
(check (vector-length (make-float-vector 5))
  =>
  5
) ;check


(let ((v (make-float-vector 5 3.14)))
  (check (vector-length v) => 5)
  (check (float-vector-ref v 0) => 3.14)
  (check (float-vector-ref v 4) => 3.14)
) ;let


(let ((v (make-float-vector 3 42)))
  (check (float-vector-ref v 0) => 42.0)
  (check (float-vector-ref v 1) => 42.0)
) ;let


(check-catch 'wrong-type-arg
  (make-float-vector 'not-a-number)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-float-vector 3 'not-a-number)
) ;check-catch


(check-report)
