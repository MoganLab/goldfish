(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; bitwise-xor
;; 计算多个整数的按位异或操作。
;;
;; 语法
;; ----
;; (bitwise-xor i1 i2 ...)
;;
;; 参数
;; ----
;; i1, i2, ... : integer?
;; 一个或多个整数，参与按位异或操作。
;;
;; 返回值
;; -----
;; integer?
;; 返回所有整数按位异或操作的结果。
;;
;; 说明
;; ----
;; 1. 对所有整数的每一位进行异或操作（相同为0，不同为1）
;; 2. 按位异或操作常用于比较位差异或实现简单的加密
;; 3. 对于任意整数 i，(bitwise-xor i i) = 0
;; 4. 对于任意整数 i，(bitwise-xor i 0) = i
;; 5. 对于任意整数 i，(bitwise-xor i -1) = (bitwise-not i)
;; 6. 按位异或操作满足交换律：(bitwise-xor i1 i2) = (bitwise-xor i2 i1)
;; 7. 按位异或操作满足结合律：(bitwise-xor i1 (bitwise-xor i2 i3)) = (bitwise-xor (bitwise-xor i1 i2) i3)
;; 8. 支持两个或多个参数，按从左到右的顺序依次进行按位异或操作
;;
;; 实现说明
;; --------
;; - bitwise-xor 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 如果考虑性能优化，可以使用 S7 Scheme 内置的 logxor 函数
;; - logxor 是 S7 的原生函数，通常比 bitwise-xor 有更好的性能
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。


;;; 基本功能测试：按位异或操作
(check (bitwise-xor 1 1) => 0)
(check (bitwise-xor #b10 #b11) => #b01) ; 2 xor 3 = 1
(check (bitwise-xor #b101010 #b110100) => #b011110) ; 42 xor 20 = 34
(check (bitwise-xor #b0 #b0) => #b0) ; 0 xor 0 = 0
(check (bitwise-xor #b1 #b1) => #b0) ; 1 xor 1 = 0
(check (bitwise-xor #b101 #b111) => #b010) ; 5 xor 7 = 2
(check (bitwise-xor #b1000 #b1001) => #b0001) ; 8 xor 9 = 1
(check (bitwise-xor #b10010101 #b01111001) => #b11101100)

;;; 边界值测试
(check (bitwise-xor 0 0) => 0)          ; 0 XOR 0 = 0
(check (bitwise-xor 0 1) => 1)          ; 0 XOR 1 = 1
(check (bitwise-xor 1 0) => 1)          ; 1 XOR 0 = 1
(check (bitwise-xor 1 1) => 0)          ; 1 XOR 1 = 0
(check (bitwise-xor -1 -1) => 0)        ; -1 XOR -1 = 0
(check (bitwise-xor -1 0) => -1)        ; -1 XOR 0 = -1
(check (bitwise-xor 0 -1) => -1)        ; 0 XOR -1 = -1

;;; 数学性质测试
(check (bitwise-xor 15 15) => 0)        ; 自反性
(check (bitwise-xor 7 3) => (bitwise-xor 3 7)) ; 交换律
(check (bitwise-xor 15 (bitwise-xor 7 3)) => (bitwise-xor (bitwise-xor 15 7) 3)) ; 结合律
(check (bitwise-xor 255 0) => 255)      ; 与0相异或得原数
(check (bitwise-xor 255 -1) => -256)    ; 与-1相异或得按位取反

;;; 二进制表示测试
(check (bitwise-xor #b10101010 #b01010101) => #b11111111)  ; 交替位模式
(check (bitwise-xor #b11110000 #b11001100) => #b00111100) ; 部分重叠
(check (bitwise-xor #b00001111 #b11110000) => #b11111111) ; 互补位模式

;;; 三个参数测试
(check (bitwise-xor 1 2 4) => 7)          ; 001 XOR 010 XOR 100 = 111
(check (bitwise-xor 1 1 1) => 1)          ; 001 XOR 001 XOR 001 = 001
(check (bitwise-xor 0 1 2) => 3)          ; 000 XOR 001 XOR 010 = 011
(check (bitwise-xor #b101 #b011 #b111) => 1) ; 101 XOR 011 XOR 111 = 001

;;; 特殊值测试
(check (bitwise-xor 2147483647 2147483647) => 0) ; 最大32位有符号整数
(check (bitwise-xor -2147483648 -2147483648) => 0) ; 最小32位有符号整数
(check (bitwise-xor 2147483647 -2147483648) => -1) ; 最大和最小整数相异或

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (bitwise-xor "string" 1)  ; 字符串参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-xor 1 'symbol)   ; 符号参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-xor 3.14 2)      ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-xor #\a 1)       ; 字符参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-xor '(1 2) 3)    ; 列表参数
) ;check-catch

;;; 多参数错误处理测试
(check-catch 'wrong-type-arg
             (bitwise-xor 1 2 3 "four")  ; 第四个参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-xor 1 2 "three" 4)  ; 第三个参数不是整数
) ;check-catch


(check-report)
