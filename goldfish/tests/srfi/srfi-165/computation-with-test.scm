;; computation-with
;; 在计算中绑定环境变量并执行后续计算。
;;
;; 语法
;; ----
;; (computation-with ((var value) ...) computation ...)
;;
;; 参数
;; ----
;; (var value) : list
;; 环境变量与其绑定值的列表。
;; computation ... : computation
;; 在绑定生效的环境中执行的计算对象。
;; 返回值
;; ----
;; computation
;; 返回在绑定环境中执行这些计算的新计算对象。
;; 说明
;; ----
;; 绑定只在该计算内部可见，不影响外层环境。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))

;; 绑定在计算内部可见
(check
  (computation-run
    (computation-with ((v 999))
      (computation-fn ((val v)) (computation-pure val))
    )
  )
  =>
  999
) ;check

;; 外层环境不受影响
(check (computation-environment-ref (make-computation-environment) v) => 100)

;; 多个绑定同时生效
(define w (make-computation-environment-variable 'y 200 #f))
(check
  (computation-run
    (computation-with ((v 1) (w 2))
      (computation-fn ((a v) (b w)) (computation-pure (list a b)))
    )
  )
  =>
  '(1 2)
) ;check

(check-report)
