(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; int-vector-ref
;; 返回整数向量中指定位置的元素。
;;
;; 语法
;; ----
;; (int-vector-ref vec k)
;;
;; 参数
;; ----
;; vec : int-vector?
;; 要访问的整数向量。
;;
;; k : integer?
;; 元素的索引（从0开始）。
;;
;; 返回值
;; ----
;; integer?
;; 向量中第k个位置的整数值。
;;
;; 注意
;; ----
;; 索引从0开始，超出范围会报错。
;;
;; 示例
;; ----
;; (int-vector-ref (int-vector 10 20 30) 0) => 10
;; (int-vector-ref (int-vector 10 20 30) 2) => 30
;;
;; 错误处理
;; ----
;; wrong-type-arg 当vec不是int-vector时
;; out-of-range 当索引越界时

(let ((v (int-vector 10 20 30 40 50)))
  (check (int-vector-ref v 0) => 10)
  (check (int-vector-ref v 1) => 20)
  (check (int-vector-ref v 2) => 30)
  (check (int-vector-ref v 4) => 50)
) ;let

(let ((v (make-int-vector 3 99)))
  (check (int-vector-ref v 0) => 99)
  (check (int-vector-ref v 1) => 99)
  (check (int-vector-ref v 2) => 99)
) ;let

(check-catch 'wrong-type-arg (int-vector-ref 'not-a-vector 0))
(check-catch 'wrong-type-arg (int-vector-ref (vector 1 2 3) 0))
(check-catch 'out-of-range (int-vector-ref (int-vector 1 2) 5))
(check-catch 'out-of-range (int-vector-ref (int-vector 1 2) -1))

(check-report)
