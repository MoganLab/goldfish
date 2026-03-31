(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; lcm
;; 计算给定有理数的最小公倍数。
;;
;; 语法
;; ----
;; (lcm reals ...)
;;
;; 参数
;; ----
;; reals : real?
;; 实数。接受零个、一个或多个参数。
;;
;; 返回值
;; ------
;; 返回最小公倍数。
;; 无参数时返回1，单参数时返回该参数本身的绝对值，参数中包含0时返回0。
;; 如果参数中含有不精确值，返回值也是不精确的。
;;
;; 错误处理
;; --------
;; type-error
;; 当参数不是实数时抛出错误。

;; 基本测试
(check (lcm) => 1)
(check (lcm 1) => 1)
(check (lcm 0) => 0)
(check (lcm -1) => 1)

(check (lcm 2 3) => 6)
(check (lcm 4 6) => 12)
(check (lcm 12 18) => 36)
(check (lcm -6 8) => 24)
(check (lcm 0 5) => 0)

(check (lcm 2 4 5) => 20)
(check (lcm 6 8 9) => 72)

(check (lcm 5/2 4) => 20)  
(check (lcm 32.0 -36.0) => 288.0)
(check (lcm 32.0 -36) => 288.0)
(check-catch 'type-error (lcm 1+2i))

(check-report)
