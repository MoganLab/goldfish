(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; bitwise-if
;; 根据掩码对两个整数进行按位条件选择操作。
;;
;; 语法
;; ----
;; (bitwise-if mask i1 i2)
;;
;; 参数
;; ----
;; mask : integer?
;; 掩码整数，决定选择 i1 还是 i2 的位。
;; - 当掩码的某位为1时，选择 i1 的对应位
;; - 当掩码的某位为0时，选择 i2 的对应位
;;
;; i1 : integer?
;; 第一个整数，当掩码对应位为1时选择该整数的位。
;;
;; i2 : integer?
;; 第二个整数，当掩码对应位为0时选择该整数的位。
;;
;; 返回值
;; -----
;; integer?
;; 返回按位条件选择的结果整数。
;;
;; 说明
;; ----
;; 1. 对每个位位置，根据掩码的值选择 i1 或 i2 的对应位
;; 2. 当掩码的某位为1时，选择 i1 的对应位
;; 3. 当掩码的某位为0时，选择 i2 的对应位
;; 4. 按位条件选择操作等价于 (bitwise-ior (bitwise-and mask i1) (bitwise-and (bitwise-not mask) i2))
;; 5. 常用于位掩码操作、位字段合并和条件位选择
;; 6. 对于任意整数 mask, i1, i2，满足以下性质：
;;    - (bitwise-if 0 i1 i2) = i2  （掩码全0，全部选择 i2）
;;    - (bitwise-if -1 i1 i2) = i1  （掩码全1，全部选择 i1）
;;    - (bitwise-if mask i i) = i    （当 i1 和 i2 相同时，结果等于 i）
;; 7. 支持所有整数类型，包括负整数
;; 8. 位操作基于整数的二进制补码表示
;;
;; 实现说明
;; --------
;; - bitwise-if 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 按位条件选择操作是位运算中的基本构建块
;; - 可以用于实现复杂的位操作逻辑
;; - 在 Goldfish Scheme 中，bitwise-if 通过 SRFI 151 库提供
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。
;; wrong-number-of-args
;; 当参数数量不是3个时抛出错误。


(check (integer-length 0) => 0)
(check (integer-length 1) => 1)     ; 1
(check (integer-length 3) => 2)     ; 11
(check (integer-length 4) => 3)     ; 100
(check (integer-length -5) => 3)    ; -101 (长度为3)
(check (integer-length #xFFFF) => 16) ; 16位二进制

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (integer-length "string")  ; 字符串参数
) ;check-catch
(check-catch 'wrong-type-arg
             (integer-length 'symbol)   ; 符号参数
) ;check-catch
(check-catch 'wrong-type-arg
             (integer-length 3.14)      ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (integer-length #\a)       ; 字符参数
) ;check-catch
(check-catch 'wrong-type-arg
             (integer-length '(1 2))    ; 列表参数
) ;check-catch

;;; 基本功能测试：bitwise-if 按位条件选择操作
(check (bitwise-if 3 1 8) => 9)  ; #b011 #001 #100 => #101
(check (bitwise-if 3 8 1) => 0)  ; #011 #100 #001 => #000
(check (bitwise-if 1 1 2) => 3)  ; #001 #001 #010 => #011
(check (bitwise-if #b00111100 #b11110000 #b00001111) => #b00110011)  ; 60 240 15 => 51

;;; 边界值测试
(check (bitwise-if 0 1 2) => 2)          ; 掩码全0，全部选择 i2
(check (bitwise-if -1 1 2) => 1)         ; 掩码全1，全部选择 i1
(check (bitwise-if 0 0 0) => 0)          ; 所有参数为0
(check (bitwise-if -1 -1 -1) => -1)      ; 所有参数为-1
(check (bitwise-if 0 255 0) => 0)        ; 掩码全0，选择 i2
(check (bitwise-if -1 0 255) => 0)       ; 掩码全1，选择 i1

;;; 二进制表示测试
(check (bitwise-if #b1010 #b1100 #b0011) => #b1001)  ; 掩码#1010，选择#1100和#0011
(check (bitwise-if #b0101 #b1100 #b0011) => #b0110)  ; 掩码#0101，选择#1100和#0011
(check (bitwise-if #b1111 #b1010 #b0101) => #b1010)  ; 掩码全1，全部选择 i1
(check (bitwise-if #b0000 #b1010 #b0101) => #b0101)  ; 掩码全0，全部选择 i2

;;; 位操作测试：验证不同掩码模式的条件选择
(check (bitwise-if #b1100 #b1010 #b0101) => #b1001)  ; 掩码#1100，选择#1010和#0101
(check (bitwise-if #b0011 #b1010 #b0101) => #b0110)  ; 掩码#0011，选择#1010和#0101
(check (bitwise-if #b1001 #b1111 #b0000) => #b1001)  ; 掩码#1001，选择#1111和#0000
(check (bitwise-if #b0110 #b1111 #b0000) => #b0110)  ; 掩码#0110，选择#1111和#0000

;;; 数学性质测试
(check (bitwise-if 5 3 7) => (bitwise-ior (bitwise-and 5 3) (bitwise-and (bitwise-not 5) 7))) ; 等价性验证
(check (bitwise-if 10 15 0) => (bitwise-ior (bitwise-and 10 15) (bitwise-and (bitwise-not 10) 0))) ; 等价性验证
(check (bitwise-if 0 5 10) => 10)        ; 掩码全0，选择 i2
(check (bitwise-if -1 5 10) => 5)        ; 掩码全1，选择 i1
(check (bitwise-if 15 8 8) => 8)         ; i1 和 i2 相同，结果等于 i1/i2

;;; 特殊值测试
(check (bitwise-if 2147483647 2147483647 0) => 2147483647) ; 最大32位有符号整数
(check (bitwise-if -2147483648 0 -2147483648) => 0) ; 最小32位有符号整数，掩码全1选择i1=0
(check (bitwise-if 2147483647 2147483647 -2147483648) => -1) ; 最大和最小整数

;;; 更多边界值测试
(check (bitwise-if 1 0 0) => 0)          ; 掩码第0位为1，但i1和i2都是0
(check (bitwise-if 1 1 1) => 1)          ; 掩码第0位为1，i1和i2都是1
(check (bitwise-if 2 2 2) => 2)          ; 掩码第1位为1，i1和i2都是2
(check (bitwise-if 4 4 4) => 4)          ; 掩码第2位为1，i1和i2都是4

;;; 负整数测试
(check (bitwise-if -1 -2 -3) => -2)      ; 掩码全1，选择 i1
(check (bitwise-if 0 -2 -3) => -3)       ; 掩码全0，选择 i2
(check (bitwise-if #b1010 -1 0) => 10)   ; 掩码#1010，选择-1和0
(check (bitwise-if #b0101 -1 0) => 5)    ; 掩码#0101，选择-1和0

;;; 大整数测试
(check (bitwise-if 4294967295 4294967295 0) => 4294967295) ; 最大32位无符号整数
(check (bitwise-if 0 4294967295 4294967295) => 4294967295) ; 掩码全0，但i1和i2相同
(check (bitwise-if 9223372036854775807 9223372036854775807 0) => 9223372036854775807) ; 最大64位有符号整数
(check (bitwise-if 0 9223372036854775807 9223372036854775807) => 9223372036854775807) ; 掩码全0，但i1和i2相同

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (bitwise-if "string" 1 2)  ; 掩码参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-if 1 "string" 2)  ; i1参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-if 1 2 "string")  ; i2参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-if 1.5 2 3)       ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bitwise-if #\a 2 3)       ; 字符参数
) ;check-catch

;;; 错误处理测试 - wrong-number-of-args
(check-catch 'wrong-number-of-args
             (bitwise-if 1)             ; 参数太少
) ;check-catch
(check-catch 'wrong-number-of-args
             (bitwise-if 1 2)           ; 参数太少
) ;check-catch
(check-catch 'wrong-number-of-args
             (bitwise-if 1 2 3 4)       ; 参数太多
) ;check-catch


(check-report)
