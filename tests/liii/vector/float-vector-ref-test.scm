(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; float-vector-ref
;; 返回浮点数向量中指定位置的元素。
;;
;; 语法
;; ----
;; (float-vector-ref vec k)
;;
;; 参数
;; ----
;; vec : float-vector?
;; 要访问的浮点数向量。
;;
;; k : integer?
;; 元素的索引（从0开始）。
;;
;; 返回值
;; ----
;; real?
;; 向量中第k个位置的浮点数值。
;;
;; 注意
;; ----
;; 索引从0开始，超出范围会报错。
;;
;; 示例
;; ----
;; (float-vector-ref (float-vector 1.0 2.0 3.0) 0) => 1.0
;; (float-vector-ref (float-vector 1.0 2.0 3.0) 2) => 3.0
;;
;; 错误处理
;; ----
;; wrong-type-arg 当vec不是float-vector时
;; out-of-range 当索引越界时

(let ((v (float-vector 10.0 20.0 30.0 40.0 50.0)))
  (check (float-vector-ref v 0) => 10.0)
  (check (float-vector-ref v 1) => 20.0)
  (check (float-vector-ref v 2) => 30.0)
  (check (float-vector-ref v 4) => 50.0))

(let ((v (make-float-vector 3 99.5)))
  (check (float-vector-ref v 0) => 99.5)
  (check (float-vector-ref v 1) => 99.5)
  (check (float-vector-ref v 2) => 99.5))

(let ((v (float-vector 1 2 3)))
  (check (float-vector-ref v 0) => 1.0)
  (check (float-vector-ref v 1) => 2.0)
  (check (float-vector-ref v 2) => 3.0))

(check-catch 'wrong-type-arg (float-vector-ref 'not-a-vector 0))
(check-catch 'wrong-type-arg (float-vector-ref (vector 1.0 2.0 3.0) 0))
(check-catch 'wrong-type-arg (float-vector-ref (int-vector 1 2) 0))
(check-catch 'out-of-range (float-vector-ref (float-vector 1.0 2.0) 5))
(check-catch 'out-of-range (float-vector-ref (float-vector 1.0 2.0) -1))

(check-report)
