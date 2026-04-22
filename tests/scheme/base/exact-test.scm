(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; exact
;; 将数字转换为精确表示形式。
;;
;; 语法
;; ----
;; (exact num)
;;
;; 参数
;; ----
;; num : number?
;; 待转换的数字，必须是实数。
;;
;; 返回值
;; ------
;; exact?
;; 输入数字的精确表示。
;;
;; 说明
;; ----
;; 1. 对于整数，返回其自身
;; 2. 对于不精确实数，返回最接近的精确有理数
;; 3. 不支持复数转换
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 参数数量不为1时抛出。
;; wrong-type-arg
;; 参数不是实数时抛出。
(check (exact 1) => 1)
(check (exact 0) => 0)
(check (exact -5) => -5)
(check (exact 1.0) => 1)
(check (exact 1.5) => 3/2)
(check (exact 0.75) => 3/4)
(check (exact -2.5) => -5/2)
(check (exact 3/4) => 3/4)
(check (exact 0.0) => 0)
(check-catch 'wrong-number-of-args
  (exact)
) ;check-catch
(check-catch 'wrong-number-of-args
  (exact 1 2)
) ;check-catch
(check-catch 'wrong-type-arg (exact 'a))
(check-catch 'wrong-type-arg
  (exact "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (exact 1.0+2.0i)
) ;check-catch
(check-report)
