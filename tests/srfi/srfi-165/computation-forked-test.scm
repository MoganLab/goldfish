;; computation-forked
;; 在环境副本上依次执行计算。
;;
;; 语法
;; ----
;; (computation-forked computation ...)
;;
;; 参数
;; ----
;; computation ... : computation
;; 待执行的计算对象列表。
;; 返回值
;; ----
;; computation
;; 返回在各自分支环境中执行这些计算的新计算对象。
;; 说明
;; ----
;; 除第一个计算外，后续计算都在环境副本上执行，
;; 因此分支内的就地修改不会互相影响。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))

;; 分支在环境副本上执行，其就地修改不影响外层绑定
(check
  (computation-run
    (computation-with ((v 'shared))
      (computation-forked
        (computation-with! (v 'branch1))
        (computation-fn ((val v)) (computation-pure val))
      )
    )
  )
  =>
  'shared
) ;check

;; 分支的修改不会泄漏到后续计算
(check
  (computation-run
    (computation-with ((v 0))
      (computation-forked
        (computation-with! (v 999))
        (computation-pure 'done)
      )
      (computation-fn ((val v)) (computation-pure val))
    )
  )
  =>
  0
) ;check


(check-report)
