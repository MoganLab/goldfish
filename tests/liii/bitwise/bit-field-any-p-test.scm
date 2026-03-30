(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; bit-field-any?
;; 检查整数指定位域中是否有任何位被设置（值为1）。
;;
;; 语法
;; ----
;; (bit-field-any? n start end)
;;
;; 参数
;; ----
;; n : integer?
;; 整数，要检查位域的整数。
;; start : integer?
;; 位域起始位置（包含），从0开始计数，0表示最低有效位（LSB）。
;; end : integer?
;; 位域结束位置（不包含），必须大于等于start。
;;
;; 返回值
;; -----
;; boolean?
;; 如果整数 n 中从 start 位到 end-1 位的位域中有任何位被设置（值为1），返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 检查整数 n 中指定范围 [start, end) 的位域中是否有任何位被设置
;; 2. 位索引从0开始，0表示最低有效位（LSB）
;; 3. 位域范围 [start, end) 是左闭右开区间
;; 4. 如果位域中至少有一个位被设置（值为1），返回 #t
;; 5. 如果位域中所有位都未被设置（值为0），返回 #f
;; 6. 对于空位域（start = end），总是返回 #f，因为没有位需要检查
;; 7. 常用于检查特定位字段中是否有任何标志被设置
;;
;; 实现说明
;; --------
;; - bit-field-any? 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用 bit-field 操作提取位域，然后检查结果是否非零
;; - 支持所有整数类型，包括负整数
;; - 注意：S7 Scheme 的 bit-field 实现与 SRFI 151 标准有所不同，可能会影响 bit-field-any? 的行为
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。
;; out-of-range
;; 当位索引超出有效范围（0-63）时抛出错误。



(check-report)
