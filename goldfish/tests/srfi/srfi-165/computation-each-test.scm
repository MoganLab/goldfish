;; computation-each
;; 依次执行多个计算，忽略各自的结果。
;;
;; 语法
;; ----
;; (computation-each computation ...)
;;
;; 参数
;; ----
;; computation ... : computation
;; 待执行的计算对象列表。
;; 返回值
;; ----
;; computation
;; 返回依次执行这些计算的新计算对象。
;; 说明
;; ----
;; 执行顺序与参数顺序一致。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


;; 依次执行
(check (computation-run (computation-each (computation-pure 1)
                          (computation-pure 2)
                          (computation-pure 3)
                        ) ;computation-each
       ) ;computation-run
  =>
  3
) ;check

;; 执行顺序
(let ((result '()))
  (computation-run (computation-each (make-computation (lambda (k) (set! result (cons 1 result)) (k (computation-pure 'void))))
                        (make-computation (lambda (k) (set! result (cons 2 result)) (k (computation-pure 'void))))
                        (make-computation (lambda (k) (set! result (cons 3 result)) (k (computation-pure 'void))))
                      ) ;computation-each
       ) ;computation-run
  (check result => '(3 2 1))
) ;let

(check-report)
