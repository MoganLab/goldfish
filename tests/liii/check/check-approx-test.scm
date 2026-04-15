(import (liii check))


(check-set-mode! 'report-failed)


;; check-approx
;; 断言两个数值在给定的相对误差和绝对误差范围内近似相等。
;;
;; 语法
;; ----
;; (check-approx expr => expected :rel-tol rel-tol :abs-tol abs-tol)
;;
;; 参数
;; ----
;; expr : number?
;; 实际计算得到的数值表达式。
;; expected : number?
;; 期望值。
;; rel-tol : number?
;; 相对误差容差，可选，默认值为 1e-12。
;; abs-tol : number?
;; 绝对误差容差，可选，默认值为 1e-12。
;;
;; 返回值
;; ----
;; 无
;; 该宏会将断言结果纳入 `check-report` 统计。
;;
;; 描述
;; ----
;; 判断条件为：
;; `abs(actual - expected) <= max(abs-tol, rel-tol * max(abs(actual), abs(expected)))`
;; 可用于浮点数、近似计算结果等场景。
;;
;; 使用场景
;; ----
;; 1. 验证浮点数四则运算结果。
;; 2. 验证三角函数、统计函数等近似数值结果。
;; 3. 当结果接近 0 时，配合 `:abs-tol` 控制误差范围。


(check-approx (+ 0.1 0.2)
  =>
  0.3
  :rel-tol
  1e-12
  :abs-tol
  1e-12
) ;check-approx
(check-approx 1.000000000001
  =>
  1.0
  :rel-tol
  2e-12
  :abs-tol
  0.0
) ;check-approx
(check-approx 1e-13
  =>
  0.0
  :rel-tol
  0.0
  :abs-tol
  1e-12
) ;check-approx


(let ((tolerance 1e-12))
  (check-approx (+ 0.1 0.2)
    =>
    0.3
    :rel-tol
    tolerance
    :abs-tol
    tolerance
  ) ;check-approx
) ;let


;; 默认参数也应可用
(check-approx (+ 0.1 0.2)
  =>
  0.30000000000000004
) ;check-approx


(check-report)
