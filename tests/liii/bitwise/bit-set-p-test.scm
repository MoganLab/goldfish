(import (liii check)
        (liii bitwise)
) ;import

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


;;; 基本功能测试：检查特定位是否设置
(check (bit-set? 1 1) => #f)        ; Binary of 1 is #b0001, bit 1 is 0
(check (bit-set? 0 1) => #t)        ; Binary of 1 is #b0001, bit 0 is 1
(check (bit-set? 3 10) => #t)       ; Binary of 10 is #b1010, bit 3 is 1
(check (bit-set? 2 6) => #t)        ; Binary of 6 is #b0110, bit 2 is 1
(check (bit-set? 0 6) => #f)        ; Binary of 6 is #b0110, bit 0 is 0

;;; 边界值测试
(check (bit-set? 0 0) => #f)        ; 0的所有位都是0
(check (bit-set? 63 0) => #f)       ; 0的所有位都是0
(check (bit-set? 0 -1) => #t)       ; -1的所有位都是1
(check (bit-set? 63 -1) => #t)      ; -1的所有位都是1
(check (bit-set? 31 -1) => #t)      ; -1的所有位都是1
(check (bit-set? 0 1) => #t)        ; 1的最低位是1
(check (bit-set? 1 1) => #f)        ; 1的第二位是0

;;; 二进制表示测试
(check (bit-set? 0 #b1010) => #f)   ; #b1010 第0位是0
(check (bit-set? 1 #b1010) => #t)   ; #b1010 第1位是1
(check (bit-set? 2 #b1010) => #f)   ; #b1010 第2位是0
(check (bit-set? 3 #b1010) => #t)   ; #b1010 第3位是1
(check (bit-set? 0 #b0101) => #t)   ; #b0101 第0位是1
(check (bit-set? 1 #b0101) => #f)   ; #b0101 第1位是0
(check (bit-set? 2 #b0101) => #t)   ; #b0101 第2位是1
(check (bit-set? 3 #b0101) => #f)   ; #b0101 第3位是0

;;; 位索引测试
(check (bit-set? 0 255) => #t)      ; 255 = #b11111111，所有位都是1
(check (bit-set? 1 255) => #t)
(check (bit-set? 2 255) => #t)
(check (bit-set? 3 255) => #t)
(check (bit-set? 4 255) => #t)
(check (bit-set? 5 255) => #t)
(check (bit-set? 6 255) => #t)
(check (bit-set? 7 255) => #t)
(check (bit-set? 8 255) => #f)      ; 255只有8位，第8位是0

;;; 特殊值测试
(check (bit-set? 30 2147483647) => #t)  ; 最大32位有符号整数，第30位是1
(check (bit-set? 31 2147483647) => #f)  ; 最大32位有符号整数，第31位是0（符号位）
(check (bit-set? 31 -2147483648) => #t) ; 最小32位有符号整数，第31位是1（符号位）
(check (bit-set? 30 -2147483648) => #f) ; 最小32位有符号整数，第30位是0
(check (bit-set? 62 9223372036854775807) => #t)  ; 最大64位有符号整数，第62位是1
(check (bit-set? 63 9223372036854775807) => #f)  ; 最大64位有符号整数，第63位是0（符号位）

;;; 负整数测试
(check (bit-set? 0 -1) => #t)       ; -1的所有位都是1
(check (bit-set? 1 -1) => #t)
(check (bit-set? 31 -1) => #t)
(check (bit-set? 63 -1) => #t)
(check (bit-set? 0 -2) => #f)       ; -2 = #b11111110，第0位是0
(check (bit-set? 1 -2) => #t)       ; -2 = #b11111110，第1位是1
(check (bit-set? 0 -3) => #t)       ; -3 = #b11111101，第0位是1
(check (bit-set? 1 -3) => #f)       ; -3 = #b11111101，第1位是0

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (bit-set? "string" 1)  ; 索引参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (bit-set? 1 "string")  ; 整数参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (bit-set? 3.14 2)      ; 浮点数索引参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bit-set? 1 3.14)      ; 浮点数整数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bit-set? #\a 1)       ; 字符索引参数
) ;check-catch
(check-catch 'wrong-type-arg
             (bit-set? 1 #\a)       ; 字符整数参数
) ;check-catch

;;; 错误处理测试 - out-of-range
(check-catch 'out-of-range
             (bit-set? -1 1)        ; 索引不能为负数
) ;check-catch
(check-catch 'out-of-range
             (bit-set? 64 1)        ; 索引不能超过63
) ;check-catch
(check-catch 'out-of-range
             (bit-set? 100 1)       ; 索引不能超过63
) ;check-catch
(check-catch 'out-of-range
             (bit-set? -100 1)      ; 索引不能为负数
) ;check-catch


(check-report)
