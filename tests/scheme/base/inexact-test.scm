(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; inexact
;; 将数字转换为不精确表示形式。
;;
;; 语法
;; ----
;; (inexact num)
;;
;; 参数
;; ----
;; num : number?
;; 待转换的数字。
;;
;; 返回值
;; ------
;; inexact?
;; 输入数字的不精确表示。
;;
;; 说明
;; ----
;; 1. 对于不精确数，返回其自身
;; 2. 对于精确数，返回最接近的不精确浮点数
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 参数数量不为1时抛出。
;; wrong-type-arg
;; 参数不是数字时抛出。
(check (inexact 1) => 1.0)
(check (inexact 0) => 0.0)
(check (inexact -5) => -5.0)
(check (inexact 3/4) => 0.75)
(check (inexact 1/2) => 0.5)
(check (inexact 1.0) => 1.0)
(check (inexact 1.5) => 1.5)
(check (inexact 0.0) => 0.0)
(check-catch 'wrong-number-of-args
  (inexact)
) ;check-catch
(check-catch 'wrong-number-of-args
  (inexact 1 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (inexact 'a)
) ;check-catch
(check-catch 'wrong-type-arg
  (inexact "hello")
) ;check-catch
(check-report)
