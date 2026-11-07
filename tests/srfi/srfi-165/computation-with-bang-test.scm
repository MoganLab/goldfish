;; computation-with!
;; 在计算中就地绑定环境变量。
;;
;; 语法
;; ----
;; (computation-with! (var value) ...)
;;
;; 参数
;; ----
;; (var value) : list
;; 环境变量与其绑定值的列表。
;; 返回值
;; ----
;; computation
;; 返回就地绑定变量后的计算对象。
;; 说明
;; ----
;; 与 computation-with 的区别是本形式会就地修改环境，
;; 对预定义变量（计算类型的静态字段）尤其可见。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))

;; 绑定生效
(check
  (computation-run
    (computation-bind
      (computation-with! (v 42))
      (lambda (_) (computation-fn ((val v)) (computation-pure val)))
    )
  )
  =>
  42
) ;check

;; 多个绑定
(define w (make-computation-environment-variable 'y 200 #f))
(check
  (computation-run
    (computation-bind
      (computation-with! (v 1) (w 2))
      (lambda (_) (computation-fn ((a v) (b w)) (computation-pure (+ a b))))
    )
  )
  =>
  3
) ;check

;; 绑定只影响被组合的计算，不影响外层环境
(check (computation-environment-ref (make-computation-environment) v) => 100)


(check-report)
