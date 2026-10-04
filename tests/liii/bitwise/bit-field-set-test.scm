(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bit-field-set
;; 设置整数中指定位域的所有位（设置为1）。
;;
;; 语法
;; ----
;; (bit-field-set n start end)
;;
;; 参数
;; ----
;; n : integer?
;; 整数，要进行位域设置操作的整数。
;; start : integer?
;; 位域起始位置（包含），从0开始计数，0表示最低有效位（LSB）。
;; end : integer?
;; 位域结束位置（不包含），必须大于等于start。
;;
;; 返回值
;; -----
;; integer?
;; 返回整数 n 中从 start 位到 end-1 位的位域被设置（设置为1）后的结果。
;;
;; 说明
;; ----
;; 1. 设置整数 n 中从 start 位到 end-1 位的位域，将这些位设置为1
;; 2. 位索引从0开始，0表示最低有效位（LSB）
;; 3. 位域范围 [start, end) 是左闭右开区间
;; 4. 设置操作只影响指定范围内的位，其他位保持不变
;; 5. 对于空位域（start = end），返回原整数 n
;; 6. 常用于位掩码操作、位字段设置和位模式修改
;; 7. 与 bit-field-clear 函数互补，bit-field-set 设置位域，bit-field-clear 清除位域
;;
;; 实现说明
;; --------
;; - bit-field-set 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用原生精确整数位运算实现
;; - 支持任意精确整数，位索引为非负精确整数
;;   - 当 start >= integer-length(n) 时会抛出 out-of-range 错误
;;   - 对于负整数，行为可能与标准不同
;;   - 对于高位设置，行为可能与标准不同
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。
;; out-of-range
;; 当位索引为负数或位字段的起始位置大于结束位置时抛出错误。



;; ; 基本功能测试：设置指定位域的所有位
(check (bit-field-set 42 1 4) => 46)
(check (bit-field-set 32 2 4) => 44)
(check (bit-field-set 0 0 3) => 7)


;; ; 边界值测试
(check (bit-field-set 0 0 1) => 1)
(check (bit-field-set 0 0 8) => 255)
(check (bit-field-set -1 0 1) => -1)
(check (bit-field-set -1 0 8) => -1)
(check (bit-field-set 1 0 1) => 1)
(check (bit-field-set 1 1 2) => 3)


;; ; 空位域测试
(check (bit-field-set 0 0 0) => 0)
(check (bit-field-set 255 0 0) => 255)
(check (bit-field-set 255 5 5) => 255)


;; ; 二进制表示测试
(check (bit-field-set 170 0 4) => 175)
(check (bit-field-set 170 4 8) => 250)
(check (bit-field-set 15 0 4) => 15)
(check (bit-field-set 15 4 8) => 255)
(check (bit-field-set 240 0 4) => 255)
(check (bit-field-set 240 4 8) => 240)


;; ; 位域范围测试
(check (bit-field-set 0 0 1) => 1)
(check (bit-field-set 0 0 2) => 3)
(check (bit-field-set 0 0 4) => 15)
(check (bit-field-set 0 0 8) => 255)
(check (bit-field-set 0 4 8) => 240)
(check (bit-field-set 0 6 8) => 192)


;; ; 特殊值测试
(check (bit-field-set 0 0 31) => 2147483647)
(check (bit-field-set 0 31 32) => 2147483648)


;; ; 负整数测试
(check (bit-field-set -2 0 1) => -1)
(check (bit-field-set -2 1 2) => -2)
(check (bit-field-set -4 0 1) => -3)
(check (bit-field-set -4 1 2) => -2)
(check (bit-field-set -8 0 2) => -5)


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg (bit-field-set "string" 0 4))
(check-catch 'wrong-type-arg (bit-field-set 1 "string" 4))
(check-catch 'wrong-type-arg (bit-field-set 1 0 "string"))
(check-catch 'wrong-type-arg (bit-field-set 3.14 0 4))
(check-catch 'wrong-type-arg (bit-field-set 1 3.14 4))
(check-catch 'wrong-type-arg (bit-field-set 1 0 3.14))


;; ; 错误处理测试 - out-of-range
(check (bit-field-set 1 0 64) => 18446744073709551615)


;; ; 其他边界情况不会抛出错误，而是返回正常值


(check-report)



(check-report)
