(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; arithmetic-shift
;; 对整数进行算术移位操作。
;;
;; 语法
;; ----
;; (arithmetic-shift i count)
;;
;; 参数
;; ----
;; i : integer?
;; 要进行移位操作的整数。
;; count : integer?
;; 移位位数，正数表示左移，负数表示右移。
;;
;; 返回值
;; -----
;; integer?
;; 返回整数 i 算术移位 count 位后的结果。
;;
;; 说明
;; ----
;; 1. 对整数 i 进行算术移位操作
;; 2. 当 count > 0 时，向左移位（相当于乘以 2^count）
;; 3. 当 count < 0 时，向右移位（相当于除以 2^|count|，保留符号位）
;; 4. 算术移位会保留整数的符号位
;;
;; 实现说明
;; --------
;; - arithmetic-shift 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 算术移位操作保持整数的符号位不变
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。

;;; 精简测试：arithmetic-shift 算术移位操作
(check (arithmetic-shift #b10 -1) => #b1) ; 2 >> 1 = 1
(check (arithmetic-shift #b10 1) => #b100) ; 2 << 1 = 4
(check (arithmetic-shift #b1000 -2) => #b10) ; 8 >> 2 = 2
(check (arithmetic-shift #b1000 2) => #b100000)
(check (arithmetic-shift #b10000000000000000 -3) => #b10000000000000)
(check (arithmetic-shift #b1000000000000000 3) => #b1000000000000000000)


(check-report)
