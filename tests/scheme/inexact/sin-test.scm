(import (liii check)
        (scheme inexact)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; sin
;; 计算给定角度的正弦值。
;;
;; 语法
;; ----
;; (sin radians)
;;
;; 参数
;; ----
;; radians : number?
;; 以弧度为单位的角度值。
;;
;; 返回值
;; ------
;; real?
;; 返回弧度角度的正弦值，当输入值为实数时值域为[-1, 1]。
;;
;; 注意
;; ----
;; 1. 计算正弦函数sin(x)
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

;; sin 基本测试
(check (sin 0) => 0)
(check (sin (/ pi 2)) => 1.0)
(check (sin pi) => 1.2246467991473532e-16)
(check (sin (* 2 pi)) => -2.4492935982947064e-16)
(check-float (sin (/ pi 4)) 0.7071067811865475)

;; 特殊角度测试
(check (sin (/ pi 6)) => 0.49999999999999994)
(check (sin (* -1 (/ pi 2))) => -1.0)
(check (sin (* 3 (/ pi 2))) => -1.0)

;; 边界测试
(check-float (sin 1000) 0.8268795405320025)
(check (sin 0.001) => 9.999998333333417e-4)
(check (sin -0.001) => -9.999998333333417e-4)

;; 复数测试
(when (not (os-windows?))
  (check (sin 1+2i) => 3.165778513216168+1.9596010414216063i)
) ;when

;; 错误处理测试
(check-catch 'wrong-type-arg (sin "hello"))
(check-catch 'wrong-number-of-args (sin))
(check-catch 'wrong-number-of-args (sin 1 2))

(check-report)
