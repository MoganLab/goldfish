(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; bitwise-not
;; 计算整数的按位取反（补码表示）。
;;
;; 语法
;; ----
;; (bitwise-not i)
;;
;; 参数
;; ----
;; i : integer?
;; 整数，要进行按位取反操作的整数。
;;
;; 返回值
;; -----
;; integer?
;; 返回整数 i 的按位取反结果。
;;
;; 说明
;; ----
;; 1. 对整数 i 的每一位进行取反操作（0 变 1，1 变 0）
;; 2. 在补码表示中，bitwise-not 等价于 (- i 1)
;; 3. 对于任意整数 i，(bitwise-not (bitwise-not i)) = i
;; 4. 对于 0，bitwise-not 返回 -1
;; 5. 对于 -1，bitwise-not 返回 0
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。


;;; 基本功能测试：按位取反操作
(check (bitwise-not 0) => -1)
(check (bitwise-not 1) => -2)
(check (bitwise-not #b1000) => -9)
(check (bitwise-not -1) => 0)

;;; 边界值测试
(check (bitwise-not 2) => -3)     ; #b10 => #b11111101
(check (bitwise-not -2) => 1)     ; #b11111110 => #b1
(check (bitwise-not 255) => -256) ; #b11111111 => #b11111111111111111111111100000000
(check (bitwise-not -256) => 255) ; #b11111111111111111111111100000000 => #b11111111

;;; 二进制表示测试
(check (bitwise-not #b1010) => -11)   ; #b1010 => #b11110101
(check (bitwise-not #b0101) => -6)    ; #b0101 => #b11111010
(check (bitwise-not #b1111) => -16)   ; #b1111 => #b11110000

;;; 特殊值测试
(check (bitwise-not 2147483647) => -2147483648)  ; 最大32位有符号整数
(check (bitwise-not -2147483648) => 2147483647)  ; 最小32位有符号整数


(check-report)
