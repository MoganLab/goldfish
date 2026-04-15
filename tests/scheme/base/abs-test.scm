(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; abs
;; 返回给定数值的绝对值。
;;
;; 语法
;; ----
;; (abs num)
;;
;; 参数
;; ----
;; num : real?
;; 任意实数，包括整数、有理数或浮点数。
;;
;; 返回值
;; ------
;; real?
;; 输入数值的绝对值，保持输入值的类型精度。
;;
;; 说明
;; ----
;; 1. 对于非负输入返回输入值本身
;; 2. 对于负输入返回其相反数
;; 3. 对于有理数返回有理数绝对值
;; 4. 对于浮点数返回浮点数绝对值
;; 5. 零的绝对值是0
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是实数时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。
;; abs 基本测试
(check (abs 0) => 0)
(check (abs 1) => 1)
(check (abs -1) => 1)
(check (abs 42) => 42)
(check (abs -42) => 42)
(check (abs 0.0) => 0.0)
(check (abs 1.5) => 1.5)
(check (abs -1.5) => 1.5)
;; 有理数测试
(check (abs 1/2) => 1/2)
(check (abs -1/2) => 1/2)
(check (abs 22/7) => 22/7)
(check (abs -22/7) => 22/7)
;; 边界测试
(check (abs 1000000000) => 1000000000)
(check (abs -1000000000) => 1000000000)
;; 零端点测试
(check (abs 0) => 0)
(check (abs 1/3) => 1/3)
(check (abs -1/3) => 1/3)
;; 错误处理测试
(check-catch 'wrong-type-arg
  (abs 1.0+2.0i)
) ;check-catch
(check-catch 'wrong-type-arg
  (abs "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (abs 'symbol)
) ;check-catch
(check-catch 'wrong-number-of-args
  (abs)
) ;check-catch
(check-catch 'wrong-number-of-args
  (abs 1 2 3)
) ;check-catch
(check-report)
