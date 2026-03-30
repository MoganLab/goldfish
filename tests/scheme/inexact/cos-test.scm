(import (liii check)
        (scheme inexact)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; cos
;; 计算给定角度的余弦值。
;;
;; 语法
;; ----
;; (cos radians)
;;
;; 参数
;; ----
;; radians : number?
;; 以弧度为单位的角度值。
;;
;; 返回值
;; ------
;; real?
;; 返回弧度角度的余弦值，当输入值为实数时值域为[-1, 1]。
;;
;; 注意
;; ----
;; 1. 计算余弦函数cos(x)
;; 2. 角度必须以弧度为单位
;; 3. 支持整数、有理数、浮点数、复数等各种数值类型
;; 4. 返回值精确度与输入值类型保持一致
;;
;; 错误处理
;; ------
;; wrong-type-arg
;; 当参数不是实数时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。

;; cos 基本测试
(check (cos 0) => 1)
(check (cos (/ pi 2)) => 6.123233995736766e-17)
(check (cos pi) => -1.0)
(check (cos (* 2 pi)) => 1.0)
(check (cos (/ pi 4)) => 0.7071067811865476)

;; 特殊角度测试
(check (cos (/ pi 3)) => 0.5000000000000001)
(check (cos (/ pi 6)) => 0.8660254037844387)
(check (cos (* -1 (/ pi 3))) => 0.5000000000000001)

;; 边界测试
(check (cos 100) => 0.8623188722876839)

;; 有理数测试
(check (cos 3/4) => 0.7316888688738209)

;; 复数测试
(when (not (os-windows?))
  (check (cos 1+2i) => 2.0327230070196656-3.0518977991518i)
) ;when

;; 错误处理测试
(check-catch 'wrong-type-arg (cos "hello"))
(check-catch 'wrong-number-of-args (cos))
(check-catch 'wrong-number-of-args (cos 1 2))

(check-report)
