(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; remainder
;; 计算两个实数相除的余数。
;;
;; 语法
;; ----
;; (remainder dividend divisor)
;;
;; 参数
;; ----
;; dividend : real?
;; 被除数
;;
;; divisor : real?
;; 除数，不能为零
;;
;; 返回值
;; ------
;; real?
;; 返回被除数除以除数的余数
;; 当参数中存在不精确数时，返回不精确数。否则，返回一个精确数。
;;
;; 错误处理
;; --------
;; division-by-zero
;; 当除数为零时抛出错误。
;; wrong-type-arg
;; 当参数不是实数时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为二时抛出错误。
(check (remainder 5 2) => 1)
(check (remainder -5 2) => -1)
(check (remainder 5 -2) => 1)
(check (remainder -5 -2) => -1)
(check (remainder 10 3) => 1)
(check (remainder -10 3) => -1)
(check (remainder 0 5) => 0)
(check (remainder 15 5) => 0)
(check (remainder 16 5) => 1)
(check (remainder 11/2 3) => 5/2)
(check-catch 'division-by-zero
  (remainder 5 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (remainder 5 "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (remainder 2.0+8.0i 5)
) ;check-catch
(check-catch 'wrong-number-of-args
  (remainder 5)
) ;check-catch
(check-catch 'wrong-number-of-args
  (remainder 5 2 3)
) ;check-catch
(check-report)
