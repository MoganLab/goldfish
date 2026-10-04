(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bit-field
;; 提取整数中指定位域的值。
;;
;; 语法
;; ----
;; (bit-field n start end)
;;
;; 参数
;; ----
;; n : integer?
;; 整数，要提取位域的整数。
;; start : integer?
;; 位域起始位置（包含），从0开始计数，0表示最低有效位（LSB）。
;; end : integer?
;; 位域结束位置（不包含），必须大于等于start。
;;
;; 返回值
;; -----
;; integer?
;; 返回整数 n 中从 start 到 end-1 位的位域值。
;;
;; 说明
;; ----
;; 1. 提取整数 n 中从 start 位到 end-1 位的位域值
;; 2. 位索引从0开始，0表示最低有效位（LSB）
;; 3. 返回的位域值是一个非负整数，表示提取的位模式
;; 4. 如果 end 超过整数的实际位数，则非负整数的超出部分视为0，负整数则符号扩展
;; 5. 位域范围 [start, end) 是左闭右开区间
;; 6. 对于整数 0，任何合法位字段都返回 0
;; 7. 常用于提取特定位字段、位掩码操作和位模式分析
;;
;; 实现说明
;; --------
;; - bit-field 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用原生精确整数位运算实现
;; - 支持任意精确整数，位索引为非负精确整数
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。
;; out-of-range
;; 当位索引为负数或位字段的起始位置大于结束位置时抛出错误。



;; ; 基本功能测试：提取位域
(check (bit-field 874 0 4) => 10)
(check (bit-field 874 3 9) => 45)
(check (bit-field 874 4 9) => 22)
(check (bit-field 874 4 10) => 54)
(check (bit-field 6 0 1) => 0)
(check (bit-field 6 1 3) => 3)
(check (bit-field 6 2 999) => 1)


;; ; 边界值测试
(check (bit-field 0 0 1) => 0)
(check (bit-field -1 0 1) => 1)
(check (bit-field 1 0 1) => 1)
(check (bit-field 1 1 2) => 0)
;; ; (check-catch 'out-of-range


;; ; 二进制表示测试
(check (bit-field 170 0 4) => 10)
(check (bit-field 240 0 4) => 0)


;; ; 位域范围测试
(check (bit-field 255 0 1) => 1)
(check (bit-field 255 0 2) => 3)
(check (bit-field 255 0 4) => 15)
(check (bit-field 255 0 8) => 255)


;; ; 特殊值测试
(check (bit-field 2147483647 0 31) => 2147483647)


;; ; 负整数测试


;; ; 超出整数长度测试
(check (bit-field 1 32 64) => 0)
(check (bit-field 255 8 16) => 0)
(check (bit-field 65535 16 32) => 0)


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg (bit-field "string" 0 4))
;; ; (check-catch 'wrong-type-arg
;; ; (check-catch 'wrong-type-arg
(check-catch 'wrong-type-arg (bit-field 3.14 0 4))
;; ; (check-catch 'wrong-type-arg
;; ; (check-catch 'wrong-type-arg


;; ; 错误处理测试 - out-of-range
(check (bit-field 0 128 129) => 0)
;; ; (check-catch 'out-of-range
;; ; (check-catch 'out-of-range
;; ; (check-catch 'out-of-range
;; ; (check-catch 'out-of-range



(check-report)
