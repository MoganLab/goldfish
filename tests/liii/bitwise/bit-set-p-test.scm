(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bit-set?
;; 检查整数中特定位是否被设置（值为1）。
;;
;; 语法
;; ----
;; (bit-set? index i)
;;
;; 参数
;; ----
;; index : integer?
;; 位索引，从0开始，表示要检查的位位置。
;; i : integer?
;; 整数，要检查位设置的整数。
;;
;; 返回值
;; -----
;; boolean?
;; 如果整数 i 的第 index 位被设置（值为1），返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 检查整数 i 的第 index 位是否为1
;; 2. 位索引从0开始，0表示最低有效位（LSB）
;; 3. 对于非负整数，检查二进制表示中特定位是否为1
;; 4. 对于负整数，检查补码表示中特定位是否为1
;; 5. 常用于位掩码检查、标志位验证和位操作
;;
;; 实现说明
;; --------
;; - bit-set? 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用 S7 Scheme 内置的位运算函数实现
;; - 支持64位整数范围，位索引范围为0到63
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。
;; out-of-range
;; 当位索引超出有效范围（0-63）时抛出错误。



;; ; 基本功能测试：检查特定位是否设置
(check (bit-set? 1 1) => #f)
(check (bit-set? 0 1) => #t)
(check (bit-set? 3 10) => #t)
(check (bit-set? 2 6) => #t)
(check (bit-set? 0 6) => #f)


;; ; 边界值测试
(check (bit-set? 0 0) => #f)
(check (bit-set? 63 0) => #f)
(check (bit-set? 0 -1) => #t)
(check (bit-set? 63 -1) => #t)
(check (bit-set? 31 -1) => #t)
(check (bit-set? 0 1) => #t)
(check (bit-set? 1 1) => #f)


;; ; 二进制表示测试
(check (bit-set? 0 10) => #f)
(check (bit-set? 1 10) => #t)
(check (bit-set? 2 10) => #f)
(check (bit-set? 3 10) => #t)
(check (bit-set? 0 5) => #t)
(check (bit-set? 1 5) => #f)
(check (bit-set? 2 5) => #t)
(check (bit-set? 3 5) => #f)


;; ; 位索引测试
(check (bit-set? 0 255) => #t)
(check (bit-set? 1 255) => #t)
(check (bit-set? 2 255) => #t)
(check (bit-set? 3 255) => #t)
(check (bit-set? 4 255) => #t)
(check (bit-set? 5 255) => #t)
(check (bit-set? 6 255) => #t)
(check (bit-set? 7 255) => #t)
(check (bit-set? 8 255) => #f)


;; ; 特殊值测试
(check (bit-set? 30 2147483647) => #t)
(check (bit-set? 31 2147483647) => #f)
(check (bit-set? 31 -2147483648) => #t)
(check (bit-set? 30 -2147483648) => #f)
(check (bit-set? 62 9223372036854775807)
  =>
  #t
) ;check
(check (bit-set? 63 9223372036854775807)
  =>
  #f
) ;check


;; ; 负整数测试
(check (bit-set? 0 -1) => #t)
(check (bit-set? 1 -1) => #t)
(check (bit-set? 31 -1) => #t)
(check (bit-set? 63 -1) => #t)
(check (bit-set? 0 -2) => #f)
(check (bit-set? 1 -2) => #t)
(check (bit-set? 0 -3) => #t)
(check (bit-set? 1 -3) => #f)


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (bit-set? "string" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-set? 1 "string")
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-set? 3.14 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-set? 1 3.14)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-set? #\a 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-set? 1 #\a)
) ;check-catch


;; ; 错误处理测试 - out-of-range
(check-catch 'out-of-range
  (bit-set? -1 1)
) ;check-catch
(check-catch 'out-of-range
  (bit-set? 64 1)
) ;check-catch
(check-catch 'out-of-range
  (bit-set? 100 1)
) ;check-catch
(check-catch 'out-of-range
  (bit-set? -100 1)
) ;check-catch



(check-report)
