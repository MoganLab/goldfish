(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; round
;; round用于返回最接近给定数的整数。
;;
;; 语法
;; ----
;; (round num)
;;
;; 参数
;; ----
;; num :real?
;; 实数值，精确的或非精确的
;;
;; 返回值
;; ------
;; 实数? -> (or (integer? integer)
;;              (real? real-with-trailing-decimal))
;; 返回最接近给定数的整数，如果两个整数同样接近，则取往远离零的方向取整。
;; 如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的。
;;
;; 说明
;; ----
;; 1. 当小数部分等于0.5时，round按照IEEE 754标准（向偶数取整）
;;    (例如：round(1.5) => 2, round(0.5) => 0, round(2.5) => 2, round(-1.5) => -2)
;; 2. 对于精确值(整数、有理数)返回精确值
;;    (例如：round(1/3) => 0, round(3/4) => 1)
;; 3. 对于非精确值(浮点数、复数)返回非精确值
;;    (例如：round(1.1) => 1.0, round(3.9) => 4.0)
;; 4. 对于实部为复数的数值，round会分别对实部和虚部四舍五入
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 如果参数不是实数，抛出错误。
;; wrong-number-of-args
;; 如果参数数量不为一，抛出错误。
(check (round 1.1) => 1.0)
(check (round 1.5) => 2.0)
(check (round 1) => 1)
(check (round 1/2) => 0)
(check (round 0) => 0)
(check (round -1) => -1)
(check (round -1.2) => -1.0)
(check (round -1.5) => -2.0)
;; 测试四舍五入到最近的整数
(check (round 0) => 0)
(check (round 0.4) => 0.0)
(check (round 0.5) => 0.0)
(check (round 0.6) => 1.0)
(check (round 1.4) => 1.0)
(check (round 1.5) => 2.0)
(check (round 1.6) => 2.0)
(check (round 2.5) => 2.0)
(check (round 3.5) => 4.0)
;; 测试负数情况
(check (round -0.4) => 0.0)
(check (round -0.5) => -0.0)
(check (round -0.6) => -1.0)
(check (round -1.4) => -1.0)
(check (round -1.5) => -2.0)
(check (round -2.5) => -2.0)
(check (round -3.5) => -4.0)
;; 测试整数边界
(check (round 2147483647) => 2147483647)
(check (round -2147483648)
  =>
  -2147483648
) ;check
;; 测试有理数情况
(check (round 1/3) => 0)
(check (round 2/3) => 1)
(check (round 3/4) => 1)
(check (round -1/3) => 0)
(check (round -2/3) => -1)
(check (round -3/4) => -1)
;; 测试错误情况
(check-catch 'wrong-type-arg
  (round "not a number")
) ;check-catch
(check-catch 'wrong-type-arg
  (round 'symbol)
) ;check-catch
(check-catch 'wrong-type-arg
  (round 1.0+2.0i)
) ;check-catch
(check-catch 'wrong-number-of-args
  (round)
) ;check-catch
(check-catch 'wrong-number-of-args
  (round 1 2)
) ;check-catch
(check-report)
