(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; truncate
;; 返回在靠近零的方向上最靠近给定数的整数。
;;
;; 语法
;; ----
;; (truncate num )
;;
;; 参数
;; ----
;; num : real?
;; 实数
;;
;; 返回值
;; ------
;; 返回在靠近零的方向上最靠近给定数的整数，即正数向下取整，负数向上取整
;; 如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果参数不是实数，抛出错误。
;; wrong-number-of-args
;; 如果参数数量不为一，抛出错误。
(check (truncate 1.1) => 1.0)
(check (truncate 1) => 1)
(check (truncate 1/2) => 0)
(check (truncate 0) => 0)
(check (truncate -1) => -1)
(check (truncate -1.2) => -1.0)
(check-catch 'wrong-type-arg
  (truncate 2.0+4.0i)
) ;check-catch
(check-catch 'wrong-type-arg
  (truncate 'hello')
) ;check-catch
(check-catch 'wrong-number-of-args
  (truncate 4 5)
) ;check-catch
(check (s7-truncate 1.1) => 1)
(check (s7-truncate -1.2) => -1)
(check-report)