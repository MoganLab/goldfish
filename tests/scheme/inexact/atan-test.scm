(import (liii check)
  (scheme inexact)
  (liii os)
) ;import
(check-set-mode! 'report-failed)
;; atan
;; 计算给定值的反正切值，或计算两个值之比值的反正切值。
;;
;; 语法
;; ----
;; (atan x [y])
;;
;; 参数
;; ----
;; x : number?
;; 当y未提供时，必须为实数，表示tan函数的值
;; 当y提供时，必须为实数，表示纵坐标
;;
;; y : real? 可选
;; 表示横坐标的实数
;;
;; 返回值
;; ------
;; real?
;; 当只有x参数时，返回x的反正切值，范围在(-π/2, π/2)内
;; 当提供x和y参数时，返回y/x的反正切值，范围在(-π, π]内
;;
;; 注意
;; ----
;; 1. 计算反正切函数arctan(x)或arctan(y/x)
;; 2. 角度以弧度为单位返回
;; 3. 双参数形式可以处理所有象限的角度
;; 4. 支持各种数值类型
;;
;; 错误处理
;; ------
;; wrong-type-arg
;; 当参数类型错误时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1或2个时抛出错误。
;; atan 基本单参数测试
(check (atan 0) => 0)
(check (atan 1) => 0.7853981633974483)
(check (atan -1) => -0.7853981633974483)
;; atan 双参数测试
(check (atan 1 1) => 0.7853981633974483)
(check (atan -1 1)
  =>
  -0.7853981633974483
) ;check
(check (atan 1 -1) => 2.356194490192345)
(check (atan -1 -1)
  =>
  -2.356194490192345
) ;check
(check (atan 0 1) => 0.0)
(check (atan 1 0) => 1.5707963267948966)
(check (atan -1 0)
  =>
  -1.5707963267948966
) ;check
;; 特殊角度测试
(check (atan (/ 1 (sqrt 3)))
  =>
  0.5235987755982989
) ;check
(check (atan 2 3) => 0.5880026035475675)
(check (atan 3 2) => 0.982793723247329)
;; 有理数测试
(check (atan 2/3) => 0.5880026035475675)
(check (atan 3/4) => 0.6435011087932844)
(check (atan 4 3) => 0.9272952180016122)
;; 边界测试
(check (atan 1000)
  =>
  1.5697963271282298
) ;check
(check (atan 0.000001)
  =>
  9.999999999996666e-7
) ;check
(check (atan -0.000001)
  =>
  -9.999999999996666e-7
) ;check
;; 复数测试
(when (not (os-windows?))
  (check (atan 1.0+2.0i)
    =>
    1.3389725222944935+0.40235947810852507i
  ) ;check
) ;when
;; 错误处理测试
(check-catch 'wrong-type-arg
  (atan "hello")
) ;check-catch
(check-catch 'wrong-number-of-args
  (atan)
) ;check-catch
(check-catch 'wrong-number-of-args
  (atan 1 2 3)
) ;check-catch
(check-catch 'wrong-type-arg
  (atan 1 "hello")
) ;check-catch
(check-report)