;; make-computation
;; 创建计算对象。
;;
;; 语法
;; ----
;; (make-computation proc)
;;
;; 参数
;; ----
;; proc : procedure
;; 接收 compute 过程的构造过程；
;; compute 接收一个子计算并返回它的结果。
;; 返回值
;; ----
;; computation
;; 返回新建的计算对象。
;; 说明
;; ----
;; 计算对象本身是一个过程，接收环境并返回计算结果。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


;; 最基本的计算
(check (computation-run (make-computation (lambda (compute) (compute (computation-pure 42)))))
  =>
  42
) ;check

;; 计算对象可以被多次求值
(let ((c (make-computation (lambda (compute) (compute (computation-pure 'first))))))
  (check (computation-run c) => 'first)
  (check (computation-run c) => 'first)
) ;let

;; 嵌套计算
(check
  (computation-run
    (make-computation
      (lambda (compute)
        (compute (make-computation (lambda (compute2) (compute2 (computation-pure 'nested)))))
      )
    )
  )
  =>
  'nested
) ;check

(check-report)
