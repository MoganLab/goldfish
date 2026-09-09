(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; int-vector-set!
;; 设置整数向量中指定位置的元素。
;;
;; 语法
;; ----
;; (int-vector-set! vec k value)
;;
;; 参数
;; ----
;; vec : int-vector?
;; 要修改的整数向量。
;;
;; k : integer?
;; 元素的索引（从0开始）。
;;
;; value : integer?
;; 要设置的新值。
;;
;; 返回值
;; ----
;; 未指定（通常为void）。
;;
;; 注意
;; ----
;; 修改是破坏性的，会改变原向量。
;; 只能设置整数值。
;;
;; 错误处理
;; ----
;; type-error 当vec不是int-vector时
;; type-error 当value不是整数时
;; out-of-range 当索引越界时


(let ((v (int-vector 1 2 3)))
  (int-vector-set! v 0 100)
  (check (int-vector-ref v 0) => 100)
  (check (int-vector-ref v 1) => 2)
  (check (int-vector-ref v 2) => 3)
) ;let


(let ((v (make-int-vector 5 0)))
  (int-vector-set! v 2 42)
  (check (int-vector-ref v 2) => 42)
  (int-vector-set! v 4 -1)
  (check (int-vector-ref v 4) => -1)
) ;let


(let ((v (int-vector 10 20 30)))
  (int-vector-set! v 1 999)
  (check v => #(10 999 30))
) ;let


(let ((v (int-vector 1 2)))
  (check-catch 'type-error (int-vector-set! v 0 'not-an-integer))
  (check-catch 'type-error (int-vector-set! v 0 3.14))
  (check-catch 'out-of-range (int-vector-set! v 5 100))
  (check-catch 'out-of-range (int-vector-set! v -1 100))
) ;let


(check-catch 'type-error (int-vector-set! (vector 1 2 3) 0 100))

;; 二维 int-vector 赋值测试
(let ((m (make-int-vector '(2 3) 0)))
  (int-vector-set! m 0 1 12)
  (int-vector-set! m 1 2 25)
  (check (int-vector-ref m 0 1) => 12)
  (check (int-vector-ref m 1 2) => 25)
  (check-catch 'out-of-range (int-vector-set! m -1 0 1))
  (check-catch 'out-of-range (int-vector-set! m 2 0 1))
  (check-catch 'out-of-range (int-vector-set! m 0 3 1))
) ;let

;; 不可变 int-vector 检查
(let ((v (immutable! (int-vector 1 2))))
  (check-catch 'immutable-error (int-vector-set! v 0 3))
) ;let


(check-report)
