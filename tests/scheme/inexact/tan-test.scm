(import (liii check) (scheme inexact))
(check-set-mode! 'report-failed)
;; tan
;; 计算给定角度的正切值。
;;
;; 语法
;; ----
;; (tan radians)
;;
;; 参数
;; ----
;; radians : number?
;; 以弧度为单位的角度值。
;;
;; 返回值
;; ------
;; real?
;; 返回弧度角度的正切值。
;;
;; 注意
;; ----
;; 1. 计算正切函数tan(x) = sin(x)/cos(x)
;; 2. 角度必须以弧度为单位
;; 3. 支持整数、有理数、浮点、复数数等各种数值类型
;; 4. 需要注意tan在π/2 + kπ处的奇点（无定义）
;;
;; 错误处理
;; ------
;; wrong-type-arg
;; 当参数不是实数时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。
;; tan 基本测试
(check (tan 0) => 0)
;; 特殊角度测试
(check (tan (/ pi 3)) => 1.7320508075688767)
;; 有理数测试
(check (tan 1/2) => 0.5463024898437905)
;; 错误处理测试
(check-catch 'wrong-type-arg (tan "hello"))
(check-catch 'wrong-number-of-args (tan))
(check-catch 'wrong-number-of-args (tan 1 2))
(check-report)