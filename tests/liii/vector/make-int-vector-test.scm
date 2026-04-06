(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; make-int-vector
;; 创建指定长度的整数向量。
;;
;; 语法
;; ----
;; (make-int-vector k)
;; (make-int-vector k fill)
;;
;; 参数
;; ----
;; k : integer?
;; 目标向量长度，必须为非负整数。
;;
;; fill : integer? 可选
;; 初始化每个元素时使用的整数值，默认为0。
;;
;; 返回值
;; ----
;; int-vector
;; 一个长度为k的新整数向量。
;;
;; 注意
;; ----
;; 创建的向量是int-vector类型，只能存储整数。
;;
;; 示例
;; ----
;; (make-int-vector 3 0) => #i(0 0 0)
;; (make-int-vector 0) => #i()
;;
;; 错误处理
;; ----
;; wrong-type-arg 当k不是合法整数时

(check (int-vector? (make-int-vector 0)) => #t)
(check (int-vector? (make-int-vector 3)) => #t)
(check (make-int-vector 3 0) => #i(0 0 0))
(check (make-int-vector 3 1) => #i(1 1 1))
(check (make-int-vector 0) => #i())
(check (vector-length (make-int-vector 5)) => 5)

(let ((v (make-int-vector 5 42)))
  (check (vector-length v) => 5)
  (check (int-vector-ref v 0) => 42)
  (check (int-vector-ref v 4) => 42)
) ;let

(check-catch 'wrong-type-arg (make-int-vector 'not-a-number))
(check-catch 'wrong-type-arg (make-int-vector 3 'not-an-integer))

(check-report)
