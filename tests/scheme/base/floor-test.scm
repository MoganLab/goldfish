(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; floor
;; 返回不大于给定数的最大整数。
;;
;; 语法
;; ----
;; (floor num )
;;
;; 参数
;; ----
;; num : real?
;; 实数
;;
;; 返回值
;; ------
;; 返回不大于给定数的最大整数
;; 如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果参数不是实数，抛出错误。
;; wrong-number-of-args
;; 如果参数数量不为一，抛出错误。
(check (floor 1.1) => 1.0)
(check (floor 1) => 1)
(check (floor 1/2) => 0)
(check (floor 0) => 0)
(check (floor -1) => -1)
(check (floor -1.2) => -2.0)
(check-catch 'wrong-type-arg
  (floor 2.0+4.0i)
) ;check-catch
(check-catch 'wrong-type-arg
  (floor 'hello')
) ;check-catch
(check-catch 'wrong-number-of-args
  (floor 4 5)
) ;check-catch
(check (s7-floor 1.1) => 1)
(check (s7-floor -1.2) => -2)
(check-report)
