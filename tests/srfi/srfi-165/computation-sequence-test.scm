;; computation-sequence
;; 依次执行计算并收集结果。
;;
;; 语法
;; ----
;; (computation-sequence computation ...)
;;
;; 参数
;; ----
;; computation ... : computation
;; 待执行的计算对象列表。
;; 返回值
;; ----
;; computation
;; 返回依次执行这些计算并收集结果的新计算对象。
;; 说明
;; ----
;; 结果按参数顺序组成列表返回。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(check (computation-run (computation-sequence (list (computation-pure 1)
                                                     (computation-pure 2)
                                                     (computation-pure 3)
                                                   ) ;list
                        ) ;computation-sequence
       ) ;computation-run
  =>
  '(1 2 3)
) ;check

;; 空序列
(check (computation-run (computation-sequence '())) => '())

;; 结果顺序与计算顺序一致
(let ((n 0))
  (computation-run
    (computation-sequence
      (list (make-computation (lambda (k) (set! n (+ n 1)) (k (computation-pure n))))
            (make-computation (lambda (k) (set! n (+ n 1)) (k (computation-pure n))))
            (make-computation (lambda (k) (set! n (+ n 1)) (k (computation-pure n))))
          )
    )
  )
  (check n => 3)
) ;let

(check-report)
