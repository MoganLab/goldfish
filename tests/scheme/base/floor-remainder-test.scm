(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; floor-remainder
;; 计算地板除法的余数（与 modulo 相同）。
;;
;; 语法
;; ----
;; (floor-remainder dividend divisor)
;;
;; 参数
;; ----
;; dividend : real?
;; 被除数。
;;
;; divisor : real?
;; 除数，不能为零。
;;
;; 返回值
;; ------
;; real?
;; 返回余数，与 modulo 结果相同。
;; 基本测试
(check (floor-remainder 17 5) => 2)
(check (floor-remainder 10 3) => 1)
(check (floor-remainder 20 4) => 0)
;; 负数测试（与 modulo 行为相同）
(check (floor-remainder -17 5) => 3)
(check (floor-remainder 17 -5) => -3)
(check (floor-remainder -17 -5) => -2)
;; 与 modulo 对比
(check (floor-remainder 10 3)
  =>
  (modulo 10 3)
) ;check
(check (floor-remainder -10 3)
  =>
  (modulo -10 3)
) ;check
(check (floor-remainder 10 -3)
  =>
  (modulo 10 -3)
) ;check
(check (floor-remainder -10 -3)
  =>
  (modulo -10 -3)
) ;check
;; 边界测试
(check (floor-remainder 0 5) => 0)
(check (floor-remainder 5 5) => 0)
(check (floor-remainder 1 5) => 1)
;; 错误测试
(check-catch 'division-by-zero
  (floor-remainder 10 0)
) ;check-catch
(check-catch 'type-error
  (floor-remainder "10" 5)
) ;check-catch
(check-report)
