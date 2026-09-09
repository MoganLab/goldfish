(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; float-vector-set!
;; 设置浮点数向量中指定位置的元素。
;;
;; 语法
;; ----
;; (float-vector-set! vec k value)
;;
;; 参数
;; ----
;; vec : float-vector?
;; 要修改的浮点数向量。
;;
;; k : integer?
;; 元素的索引（从0开始）。
;;
;; value : real?
;; 要设置的新值。
;;
;; 返回值
;; ----
;; 未指定（通常为void）。
;;
;; 注意
;; ----
;; 修改是破坏性的，会改变原向量。
;; 整数会被自动转换为浮点数。
;;
;; 错误处理
;; ----
;; type-error 当vec不是float-vector时
;; type-error 当value不是实数时
;; out-of-range 当索引越界时


(let ((v (float-vector 1.0 2.0 3.0)))
  (float-vector-set! v 0 100.0)
  (check (float-vector-ref v 0) => 100.0)
  (check (float-vector-ref v 1) => 2.0)
  (check (float-vector-ref v 2) => 3.0)
) ;let


(let ((v (make-float-vector 5 0.0)))
  (float-vector-set! v 2 42.0)
  (check (float-vector-ref v 2) => 42.0)
  (float-vector-set! v 4 -1.5)
  (check (float-vector-ref v 4) => -1.5)
) ;let


(let ((v (float-vector 10.0 20.0 30.0)))
  (float-vector-set! v 1 999)
  (check (float-vector-ref v 1) => 999.0)
  (check v => #(10.0 999.0 30.0))
) ;let


(let ((v (float-vector 1.0 2.0)))
  (check-catch 'type-error (float-vector-set! v 0 'not-a-number))
  (check-catch 'out-of-range (float-vector-set! v 5 100.0))
  (check-catch 'out-of-range (float-vector-set! v -1 100.0))
) ;let


(check-catch 'type-error (float-vector-set! (vector 1.0 2.0 3.0) 0 100.0))

(let ((v (float-vector 1.0 2.0 3.0)))
  (define (foo vec idx)
    (float-vector-set! vec idx (if #t 1.0 (float-vector-ref vec idx)))
  ) ;define
  (check-catch 'out-of-range (foo v 100000000))
  (check-catch 'out-of-range (foo v -1))
) ;let

;; 二维 float-vector 赋值测试
(let ((m (make-float-vector '(2 3) 0.0)))
  (float-vector-set! m 0 1 12.5)
  (float-vector-set! m 1 2 25.0)
  (check (float-vector-ref m 0 1) => 12.5)
  (check (float-vector-ref m 1 2) => 25.0)
  (check-catch 'out-of-range (float-vector-set! m -1 0 1.0))
  (check-catch 'out-of-range (float-vector-set! m 2 0 1.0))
  (check-catch 'out-of-range (float-vector-set! m 0 3 1.0))
) ;let

;; 不可变 float-vector 检查
(let ((v (immutable! (float-vector 1.0 2.0))))
  (check-catch 'immutable-error (float-vector-set! v 0 3.0))
) ;let


(check-report)
