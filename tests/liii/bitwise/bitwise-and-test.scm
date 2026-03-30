(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; bitwise-and
;; 计算多个整数的按位与操作。
;;
;; 语法
;; ----
;; (bitwise-and i1 i2 ...)
;;
;; 参数
;; ----
;; i1, i2, ... : integer?
;; 一个或多个整数，参与按位与操作。
;;
;; 返回值
;; -----
;; integer?
;; 返回所有整数按位与操作的结果。
;;
;; 说明
;; ----
;; 1. 对所有整数的每一位进行与操作（都为1时结果为1，否则为0）
;; 2. 按位与操作常用于提取特定位或掩码操作
;; 3. 对于任意整数 i，(bitwise-and i i) = i
;; 4. 对于任意整数 i，(bitwise-and i 0) = 0
;; 5. 对于任意整数 i，(bitwise-and i -1) = i
;; 6. 按位与操作满足交换律：(bitwise-and i1 i2) = (bitwise-and i2 i1)
;; 7. 按位与操作满足结合律：(bitwise-and i1 (bitwise-and i2 i3)) = (bitwise-and (bitwise-and i1 i2) i3)
;; 8. 支持两个或多个参数，按从左到右的顺序依次进行按位与操作
;;
;; 实现说明
;; --------
;; - bitwise-and 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 在 Goldfish Scheme 中，bitwise-and 直接定义为 logand 的别名
;; - logand 是 S7 的原生函数，支持多个参数的按位与操作
;; - 使用 S7 内置的 logand 函数提供更好的性能和兼容性
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。


;;; 基本功能测试：按位与操作
(check (bitwise-and 5 3) => 1)  ; 5 (101) AND 3 (011) = 1 (001)
(check (bitwise-and 8 4) => 0)  ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b101 #b011) => 1)  ; 5 (101) AND 3 (011) = 1 (001)
(check (bitwise-and #b1000 #b0100) => 0) ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b1100 #b1010) => 8)

;;; 边界值测试
(check (bitwise-and 0 0) => 0)          ; 0 AND 0 = 0
(check (bitwise-and 0 1) => 0)          ; 0 AND 1 = 0
(check (bitwise-and 1 0) => 0)          ; 1 AND 0 = 0
(check (bitwise-and 1 1) => 1)          ; 1 AND 1 = 1
(check (bitwise-and -1 -1) => -1)       ; -1 AND -1 = -1
(check (bitwise-and -1 0) => 0)         ; -1 AND 0 = 0
(check (bitwise-and 0 -1) => 0)         ; 0 AND -1 = 0

;;; 数学性质测试
(check (bitwise-and 15 15) => 15)       ; 自反性
(check (bitwise-and 7 3) => (bitwise-and 3 7)) ; 交换律
(check (bitwise-and 15 (bitwise-and 7 3)) => (bitwise-and (bitwise-and 15 7) 3)) ; 结合律
(check (bitwise-and 255 0) => 0)        ; 与0相与得0
(check (bitwise-and 255 -1) => 255)     ; 与-1相与得原数

;;; 二进制表示测试
(check (bitwise-and #b10101010 #b01010101) => 0)  ; 交替位模式
(check (bitwise-and #b11110000 #b11001100) => #b11000000) ; 部分重叠
(check (bitwise-and #b11111111 #b00001111) => #b00001111) ; 掩码提取低4位
(check (bitwise-and #b11111111 #b11110000) => #b11110000) ; 掩码提取高4位

;;; 特殊值测试
(check (bitwise-and 2147483647 2147483647) => 2147483647) ; 最大32位有符号整数
(check (bitwise-and -2147483648 -2147483648) => -2147483648) ; 最小32位有符号整数
(check (bitwise-and 2147483647 -2147483648) => 0) ; 最大和最小整数相与

;;; 三个参数测试
(check (bitwise-and 1 2 3) => 0)          ; 001 & 010 & 011 = 000
(check (bitwise-and 7 3 5) => 1)          ; 111 & 011 & 101 = 001
(check (bitwise-and 15 7 3) => 3)         ; 1111 & 0111 & 0011 = 0011
(check (bitwise-and #b101 #b011 #b111) => 1) ; 101 & 011 & 111 = 001
(check (bitwise-and #b1100 #b1010 #b0110) => 0) ; 1100 & 1010 & 0110 = 0000
(check (bitwise-and 255 127 63) => 63)    ; 11111111 & 01111111 & 00111111 = 00111111
(check (bitwise-and -1 -1 -1) => -1)      ; 全1相与
(check (bitwise-and 0 1 2) => 0)          ; 包含0的相与
(check (bitwise-and 1 1 1) => 1)          ; 全1相与
(check (bitwise-and 2 2 2) => 2)          ; 相同数相与

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (bitwise-and "string" 1)  ; 字符串参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-and 1 'symbol)   ; 符号参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-and 3.14 2)      ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-and #\a 1)       ; 字符参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-and '(1 2) 3)    ; 列表参数
) ;check-catch

;;; 多参数错误处理测试
(check-catch 'wrong-type-arg
             (bitwise-and 1 2 3 "four")  ; 第四个参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-and 1 2 "three" 4)  ; 第三个参数不是整数
) ;check-catch


(check-report)
