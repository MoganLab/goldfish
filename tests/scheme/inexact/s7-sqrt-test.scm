(import (liii check) (scheme inexact))
(check-set-mode! 'report-failed)
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
;; ------
;; number?
;; 返回z的平方根。
;;
;; 注意
;; ----
;; 这是 s7 Scheme 内置的 sqrt 实现，与 scheme inexact 中的 sqrt 行为略有不同。
;; scheme inexact 中的 sqrt 对负数的精确值有特殊处理。
;;
;; 示例
;; ----
;; (s7-sqrt 9) => 3
;; (s7-sqrt 25.0) => 5.0
;;
;; 错误处理
;; ------
;; wrong-type-arg
;; 当参数不是数值时抛出错误。
;; s7-sqrt 基本测试
(check (s7-sqrt 9) => 3)
(check (s7-sqrt 25.0) => 5.0)
(check (s7-sqrt 9/4) => 3/2)
(check (< (abs (- (s7-sqrt 2.0) 1.4142135623730951)) 1e-10)
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
(check-catch 'wrong-type-arg (s7-sqrt "hello"))
(check-catch 'wrong-number-of-args (s7-sqrt))
(check-report)