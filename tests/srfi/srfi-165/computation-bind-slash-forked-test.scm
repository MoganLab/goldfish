;; computation-bind/forked
;; 在环境副本上绑定计算结果。
;;
;; 语法
;; ----
;; (computation-bind/forked computation proc ...)
;;
;; 参数
;; ----
;; computation : computation
;; 提供结果的计算对象。
;; proc : procedure
;; 接收结果并返回新计算对象的过程。
;; 返回值
;; ----
;; computation
;; 返回在环境副本上完成绑定的计算对象。
;; 说明
;; ----
;; 与 computation-bind 的区别是：
;; 被绑定的计算在环境副本上执行，其就地修改不会影响外层环境。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))

;; 被绑定的计算在环境副本上执行，其就地修改不影响外层绑定
(check
  (computation-run
    (computation-with ((v 'original))
      (computation-bind/forked
        (computation-with! (v 'changed))
        (lambda (_) (computation-fn ((val v)) (computation-pure val)))
      )
    )
  )
  =>
  'original
) ;check


(check-report)
