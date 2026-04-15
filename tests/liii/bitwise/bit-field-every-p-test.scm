(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bit-field-every?
;; 检查整数指定位域中是否所有位都被设置（值为1）。
;;
;; 语法
;; ----
;; (bit-field-every? n start end)
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
;; 如果整数 n 中从 start 位到 end-1 位的位域中所有位都被设置（值为1），返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 检查整数 n 中指定范围 [start, end) 的位域中是否所有位都被设置
;; 2. 位索引从0开始，0表示最低有效位（LSB）
;; 3. 位域范围 [start, end) 是左闭右开区间
;; 4. 如果位域中所有位都被设置（值为1），返回 #t
;; 5. 如果位域中至少有一个位未被设置（值为0），返回 #f
;; 6. 对于空位域（start = end），总是返回 #t，因为没有位需要检查（空位域满足所有位都被设置的条件）
;; 7. 常用于验证特定位字段中是否所有标志都被设置
;; 8. 与 bit-field-any? 函数互补，bit-field-every? 检查是否所有位都设置，而 bit-field-any? 检查是否有任何位设置
;;
;; 实现说明
;; --------
;; - bit-field-every? 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用 bit-field 操作提取位域，然后检查结果是否等于该位域的最大可能值
;; - 支持所有整数类型，包括负整数
;; - 注意：S7 Scheme 的 bit-field 实现与 SRFI 151 标准有所不同，可能会影响 bit-field-every? 的行为
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。
;; out-of-range
;; 当位索引超出有效范围（0-63）时抛出错误。


;; ; 基本功能测试：检查位域中是否所有位设置
(check (bit-field-every? 94 1 5) => #t)
(check (bit-field-every? 90 1 5) => #f)


;; ; 边界值测试
(check (bit-field-every? 0 0 1) => #f)
(check (bit-field-every? 0 0 8) => #f)
(check (bit-field-every? -1 0 1) => #t)
(check (bit-field-every? -1 0 8) => #t)
(check (bit-field-every? 1 0 1) => #t)
(check (bit-field-every? 1 1 2) => #f)


;; ; 空位域测试
(check (bit-field-every? 255 0 0) => #t)
(check (bit-field-every? 255 5 5) => #t)
(check (bit-field-every? 0 0 0) => #t)


;; ; 二进制表示测试
(check (bit-field-every? 170 0 4) => #f)
(check (bit-field-every? 170 4 8) => #f)
(check (bit-field-every? 15 0 4) => #t)
(check (bit-field-every? 15 4 8) => #f)
(check (bit-field-every? 240 0 4) => #f)
(check (bit-field-every? 240 4 8) => #t)


;; ; 位域范围测试
(check (bit-field-every? 255 0 1) => #t)
(check (bit-field-every? 255 0 2) => #t)
(check (bit-field-every? 255 0 4) => #t)
(check (bit-field-every? 255 0 8) => #t)
(check (bit-field-every? 254 0 1) => #f)
(check (bit-field-every? 254 0 2) => #f)
(check (bit-field-every? 254 1 2) => #t)


;; ; 特殊值测试
(check (bit-field-every? 2147483647 0 31)
  =>
  #t
) ;check
(check (bit-field-every? 2147483647 31 32)
  =>
  #f
) ;check
(check (bit-field-every? -2147483648 31 32)
  =>
  #t
) ;check


;; ; 负整数测试
(check (bit-field-every? -1 0 8) => #t)
(check (bit-field-every? -2 0 1) => #f)
(check (bit-field-every? -2 1 2) => #t)
(check (bit-field-every? -3 0 1) => #t)
(check (bit-field-every? -3 1 2) => #f)


;; ; 与 bit-field-any? 的互补关系测试
(check (bit-field-every? 10 0 4)
  =>
  (not (bit-field-any? (bitwise-not 10) 0 4)
  ) ;not
) ;check
(check (bit-field-every? 5 0 4)
  =>
  (not (bit-field-any? (bitwise-not 5) 0 4)
  ) ;not
) ;check


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (bit-field-every? "string" 0 4)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-field-every? 1 "string" 4)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-field-every? 1 0 "string")
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-field-every? 3.14 0 4)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-field-every? 1 3.14 4)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-field-every? 1 0 3.14)
) ;check-catch


;; ; 错误处理测试 - out-of-range
;; ; 注意：S7 Scheme 的 bit-field-every? 实现与 SRFI 151 标准有所不同
;; ; 只有结束索引超过63时会抛出 out-of-range 错误
(check-catch 'out-of-range
  (bit-field-every? 1 0 64)
) ;check-catch


;; ; 其他边界情况不会抛出错误，而是返回正常值
;; ; 注意：S7 Scheme 的 bit-field-every? 对边界情况返回 #f
(check (bit-field-every? 1 -1 4) => #f)
(check (bit-field-every? 1 0 -1) => #f)
(check (bit-field-every? 1 64 65) => #f)
(check (bit-field-every? 1 5 4) => #f)



(check-report)
