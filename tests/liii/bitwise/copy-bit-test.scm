(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; copy-bit
;; 复制特定位的设置到目标整数中。
;;
;; 语法
;; ----
;; (copy-bit index i boolean)
;;
;; 参数
;; ----
;; index : integer?
;; 位索引，从0开始，表示要复制设置的位位置。
;; i : integer?
;; 目标整数，要修改位设置的整数。
;; boolean : any
;; 指定要设置的位值（非零值表示设置位为1，零值表示清除位为0）。
;;
;; 返回值
;; -----
;; integer?
;; 返回修改后的整数，其中第 index 位被设置为指定的值。
;;
;; 说明
;; ----
;; 1. 将目标整数 i 的第 index 位设置为指定的值
;; 2. 当 boolean 为非零值时，将第 index 位设置为1
;; 3. 当 boolean 为零值时，将第 index 位设置为0
;; 4. 位索引从0开始，0表示最低有效位（LSB）
;; 5. 支持任意精确整数，位索引为非负精确整数
;; 6. 常用于位掩码操作、位字段设置和位操作
;;
;; 实现说明
;; --------
;; - copy-bit 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用原生精确整数位运算实现
;; - 支持任意精确整数，位索引为非负精确整数
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当 index 或 i 参数不是整数时抛出错误。
;; out-of-range
;; 当位索引为负数或位字段的起始位置大于结束位置时抛出错误。



;; ; 基本功能测试：复制特定位的设置
(check (copy-bit 0 0 #t) => 1)
(check (copy-bit 2 0 #t) => 4)
(check (copy-bit 2 15 #f) => 11)
(check (copy-bit 62 0 #t) => 4611686018427387904)
(check (copy-bit 63 1 #t) => 9223372036854775809)
(check (copy-bit 63 -1 #f) => -9223372036854775809)


;; ; 边界值测试
(check (copy-bit 0 0 #t) => 1)
(check (copy-bit 0 0 #f) => 0)
(check (copy-bit 0 1 #f) => 0)
(check (copy-bit 0 1 #t) => 1)
(check (copy-bit 0 -1 #f) => -2)
(check (copy-bit 0 -1 #t) => -1)


;; ; 二进制表示测试
(check (copy-bit 0 10 #t) => 11)
(check (copy-bit 1 10 #f) => 8)
(check (copy-bit 2 10 #t) => 14)
(check (copy-bit 3 10 #f) => 2)
(check (copy-bit 0 5 #f) => 4)
(check (copy-bit 1 5 #t) => 7)


;; ; 位索引测试
(check (copy-bit 0 255 #f) => 254)
(check (copy-bit 1 255 #f) => 253)
(check (copy-bit 2 255 #f) => 251)
(check (copy-bit 3 255 #f) => 247)
(check (copy-bit 4 255 #f) => 239)
(check (copy-bit 5 255 #f) => 223)
(check (copy-bit 6 255 #f) => 191)
(check (copy-bit 7 255 #f) => 127)


;; ; 特殊值测试
(check (copy-bit 31 2147483647 #t) => 4294967295)
(check (copy-bit 31 -2147483648 #f) => -4294967296)
(check (copy-bit 63 9223372036854775807 #t) => 18446744073709551615)
(check (copy-bit 63 -9223372036854775808 #f) => -18446744073709551616)


;; ; 负整数测试
(check (copy-bit 0 -2 #t) => -1)
(check (copy-bit 1 -1 #f) => -3)
(check (copy-bit 0 -3 #t) => -3)
(check (copy-bit 1 -3 #f) => -3)


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg (copy-bit "string" 1 #t))
(check-catch 'wrong-type-arg (copy-bit 1 "string" #t))
(check-catch 'wrong-type-arg (copy-bit 3.14 2 #t))
(check-catch 'wrong-type-arg (copy-bit 1 3.14 #t))
(check-catch 'wrong-type-arg (copy-bit #\a 1 #t))
(check-catch 'wrong-type-arg (copy-bit 1 #\a #t))


;; ; 错误处理测试 - out-of-range
(check (copy-bit 64 -1 #f) => -18446744073709551617)
(check (copy-bit 10000 -1 #f) => (- (+ (expt 2 10000) 1)))
(check-catch 'out-of-range (copy-bit -1 1 #t))



(check-report)
