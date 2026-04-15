(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; make-vector
;; 创建指定长度的向量。
;;
;; 语法
;; ----
;; (make-vector k)
;; (make-vector k fill)
;;
;; 参数
;; ----
;; k : integer?
;; 目标向量长度，必须为非负整数。
;;
;; fill : any? 可选
;; 初始化每个元素时使用的值。
;;
;; 返回值
;; ----
;; vector
;; 一个长度为k的新向量。
;;
;; 注意
;; ----
;; 当提供fill时，所有位置都会初始化为相同的值。
;;
;; 示例
;; ----
;; (make-vector 3 0) => #(0 0 0)
;; (make-vector 0) => #()
;;
;; 错误处理
;; ----
;; wrong-type-arg 当k不是合法整数时


(check (vector-length (make-vector 0))
  =>
  0
) ;check
(check (vector-length (make-vector 3))
  =>
  3
) ;check
(check (make-vector 3 0) => #(0 0 0))
(check (make-vector 2 'a) => #(a a))
(check (make-vector 1 "hello")
  =>
  #("hello")
) ;check
(check (make-vector 0) => #())
(check (vector-length (make-vector 1))
  =>
  1
) ;check


(let ((v (make-vector 5 3.14)))
  (check (vector-length v) => 5)
  (check (vector-ref v 0) => 3.14)
  (check (vector-ref v 4) => 3.14)
) ;let


(check-catch 'wrong-type-arg
  (make-vector 'not-a-number)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-vector -1)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-vector -1 0)
) ;check-catch


(check-report)
