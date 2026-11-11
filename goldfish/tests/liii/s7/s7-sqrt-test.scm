;; s7-sqrt
;; s7 内置的平方根函数。
;;
;; 语法
;; ----
;; (s7-sqrt z)
;;
;; 参数
;; ----
;; z : number?
;; 被开方数。
;;
;; 返回值
;; ----
;; number?
;; 返回z的平方根。
;;
;; 注意
;; ----
;; 这是 s7 Scheme 内置的 sqrt 实现。
;;
;; (s7-sqrt 9) => 3
;; (s7-sqrt 25.0) => 5.0
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是数值时抛出错误。
;;
;; 说明
;; ----
;; 1. 与 `sqrt` 的区别：`sqrt` 先把精确负数转成不精确数再计算，保证负数一律返回
;;    复数结果（(sqrt -1) => 0.0+1.0i）；`s7-sqrt` 把参数原样交给 S7 内建计算。
;; 2. 对正数而言两者结果一致（(sqrt 9) => 3，(s7-sqrt 9) => 3）。
;; 3. `s7-sqrt` 不做 R7RS 的参数检查，非法参数会抛出 S7 原生错误。
;;
;; 选择建议：常规代码用 `sqrt`；参数确定非负且只追求 S7 原生性能时用 `s7-sqrt`。
;;
;; 参见: gf doc scheme/inexact "sqrt"

(import (liii check) (liii s7))
(check-set-mode! 'report-failed)

(check (s7-sqrt 9) => 3)
(check (s7-sqrt 25.0) => 5.0)
(check (s7-sqrt 9/4) => 3/2)
(check
  (< (abs (- (s7-sqrt 2.0) 1.4142135623730951)) 1e-10)
  =>
  #t
) ;check
;; s7-sqrt 边界测试
(check (s7-sqrt 0) => 0)
(check (s7-sqrt 0.0) => 0.0)
(check (s7-sqrt 1) => 1)
(check (s7-sqrt 1.0) => 1.0)
;; s7-sqrt 精度测试
(check (exact? (s7-sqrt 4)) => #t)
(check (exact? (s7-sqrt 4.0)) => #f)
;; 错误处理测试
(check-catch 'type-error (s7-sqrt "hello"))
(check-catch 'wrong-number-of-args (s7-sqrt))

(check-report)
