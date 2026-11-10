;; computation-local
;; 在局部环境中执行计算。
;;
;; 语法
;; ----
;; (computation-local updater computation)
;;
;; 参数
;; ----
;; updater : procedure
;; 接收环境并返回新环境的过程。
;; computation : computation
;; 待执行的计算对象。
;; 返回值
;; ----
;; computation
;; 返回在更新后环境中执行 computation 的新计算对象。
;; 说明
;; ----
;; 常与 computation-environment-update / computation-environment-copy 组合使用。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 'global-val #f))
(define env (make-computation-environment))

;; updater 产生的新环境对内部计算可见
(check
  (computation-run
    (computation-local (lambda (e) (computation-environment-update e v 'local-val))
      (computation-bind (computation-ask)
        (lambda (e) (computation-pure (computation-environment-ref e v)))
      )
    )
  )
  =>
  'local-val
) ;check

;; 外部环境不受影响
(check (computation-environment-ref env v) => 'global-val)

(check-report)
