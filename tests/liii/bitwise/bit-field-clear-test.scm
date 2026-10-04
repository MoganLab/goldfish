(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bit-field-clear
;; 清除整数中指定位域的所有位（设置为0）。
;;
;; 语法
;; ----
;; (bit-field-clear n start end)
;;
;; 参数
;; ----
;; n : integer?
;; 整数，要进行位域清除操作的整数。
;; start : integer?
;; 位域起始位置（包含），从0开始计数，0表示最低有效位（LSB）。
;; end : integer?
;; 位域结束位置（不包含），必须大于等于start。
;;
;; 返回值
;; -----
;; integer?
;; 返回整数 n 中从 start 位到 end-1 位的位域被清除（设置为0）后的结果。
;;
;; 说明
;; ----
;; 1. 清除整数 n 中从 start 位到 end-1 位的位域，将这些位设置为0
;; 2. 位索引从0开始，0表示最低有效位（LSB）
;; 3. 位域范围 [start, end) 是左闭右开区间
;; 4. 清除操作只影响指定范围内的位，其他位保持不变
;; 5. 对于空位域（start = end），返回原整数 n
;; 6. 常用于位掩码操作、位字段清除和位模式修改
;; 7. 与 bit-field-set 函数互补，bit-field-clear 清除位域，bit-field-set 设置位域
;;
;; 实现说明
;; --------
;; - bit-field-clear 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用原生精确整数位运算实现
;; - 支持任意精确整数，位索引为非负精确整数
;;   - 当 start >= integer-length(n) 时会抛出 out-of-range 错误
;;   - 对于负整数，行为可能与标准不同
;;   - 对于高位清除，行为可能与标准不同
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。
;; out-of-range
;; 当位索引为负数或位字段的起始位置大于结束位置时抛出错误。



;; ; 基本功能测试：清除指定位域的所有位
(check (bit-field-clear 42 1 4) => 32)
(check (bit-field-clear 63 2 4) => 51)
(check (bit-field-clear 42 0 6) => 0)
(check (bit-field-clear 204 2 6) => 192)


;; ; 边界值测试
(check (bit-field-clear 0 0 1) => 0)
(check (bit-field-clear 0 0 8) => 0)
(check (bit-field-clear -1 0 1) => -2)
(check (bit-field-clear -1 0 8) => -256)
(check (bit-field-clear 1 0 1) => 0)
(check (bit-field-clear 1 1 2) => 1)


;; ; 空位域测试
(check (bit-field-clear 255 0 0) => 255)
(check (bit-field-clear 255 5 5) => 255)
(check (bit-field-clear 0 0 0) => 0)


;; ; 二进制表示测试
(check (bit-field-clear 170 0 4) => 160)
(check (bit-field-clear 170 4 8) => 10)
(check (bit-field-clear 15 0 4) => 0)
(check (bit-field-clear 15 4 8) => 15)
(check (bit-field-clear 240 0 4) => 240)
(check (bit-field-clear 240 4 8) => 0)


;; ; 位域范围测试
(check (bit-field-clear 255 0 1) => 254)
(check (bit-field-clear 255 0 2) => 252)
(check (bit-field-clear 255 0 4) => 240)
(check (bit-field-clear 255 0 8) => 0)
(check (bit-field-clear 255 4 8) => 15)
(check (bit-field-clear 255 6 8) => 63)


;; ; 特殊值测试
(check (bit-field-clear 2147483647 0 31) => 0)
(check (bit-field-clear 2147483647 31 32) => 2147483647)


;; ; 负整数测试
(check (bit-field-clear -1 0 1) => -2)
(check (bit-field-clear -1 0 8) => -256)
(check (bit-field-clear -2 0 1) => -2)
(check (bit-field-clear -2 1 2) => -4)
(check (bit-field-clear -3 0 1) => -4)
(check (bit-field-clear -3 1 2) => -3)


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg (bit-field-clear "string" 0 4))
(check-catch 'wrong-type-arg (bit-field-clear 1 "string" 4))
(check-catch 'wrong-type-arg (bit-field-clear 1 0 "string"))
(check-catch 'wrong-type-arg (bit-field-clear 3.14 0 4))
(check-catch 'wrong-type-arg (bit-field-clear 1 3.14 4))
(check-catch 'wrong-type-arg (bit-field-clear 1 0 3.14))


;; ; 错误处理测试 - out-of-range
(check (bit-field-clear 1 0 64) => 0)


;; ; 其他边界情况不会抛出错误，而是返回正常值



(check-report)
