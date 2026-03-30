(import (liii check)
        (liii bitwise)
) ;import

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
;; 5. 支持64位整数范围，位索引范围为0到63
;; 6. 常用于位掩码操作、位字段设置和位操作
;;
;; 实现说明
;; --------
;; - copy-bit 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用 S7 Scheme 内置的位运算函数实现
;; - 支持64位整数范围，位索引范围为0到63
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当 index 或 i 参数不是整数时抛出错误。
;; out-of-range
;; 当位索引超出有效范围（0-63）时抛出错误。


;;; 基本功能测试：复制特定位的设置
(check (copy-bit 0 0 #t) => #b1)         ; 将0的第0位设置为1，结果是 #b1
(check (copy-bit 2 0 #t) => #b100)       ; 将0的第2位设置为1，结果是 #b100
(check (copy-bit 2 #b1111 #f) => #b1011) ; 将 #b1111 的第2位设置为0，结果是 #b1011
(check (copy-bit 62 0 #t) => #x4000000000000000) ; 设置第62位
(check (copy-bit 63 1 #t) => #x8000000000000001) ; 设置第63位
(check (copy-bit 63 -1 #f) => #x7FFFFFFFFFFFFFFF) ; 清除第63位

;;; 边界值测试
(check (copy-bit 0 0 #t) => 1)           ; 0的第0位设置为1
(check (copy-bit 0 0 #f) => 0)           ; 0的第0位设置为0
(check (copy-bit 0 1 #f) => 0)           ; 1的第0位设置为0
(check (copy-bit 0 1 #t) => 1)           ; 1的第0位设置为1
(check (copy-bit 0 -1 #f) => -2)         ; -1的第0位设置为0
(check (copy-bit 0 -1 #t) => -1)         ; -1的第0位设置为1

;;; 二进制表示测试
(check (copy-bit 0 #b1010 #t) => #b1011) ; #b1010 第0位设置为1，结果是 #b1011
(check (copy-bit 1 #b1010 #f) => #b1000) ; #b1010 第1位设置为0，结果是 #b1000
(check (copy-bit 2 #b1010 #t) => #b1110) ; #b1010 第2位设置为1，结果是 #b1110
(check (copy-bit 3 #b1010 #f) => #b0010) ; #b1010 第3位设置为0，结果是 #b0010
(check (copy-bit 0 #b0101 #f) => #b0100) ; #b0101 第0位设置为0，结果是 #b0100
(check (copy-bit 1 #b0101 #t) => #b0111) ; #b0101 第1位设置为1，结果是 #b0111

;;; 位索引测试
(check (copy-bit 0 255 #f) => 254)       ; 255 = #b11111111，第0位设置为0，结果是254
(check (copy-bit 1 255 #f) => 253)       ; 255 = #b11111111，第1位设置为0，结果是253
(check (copy-bit 2 255 #f) => 251)       ; 255 = #b11111111，第2位设置为0，结果是251
(check (copy-bit 3 255 #f) => 247)       ; 255 = #b11111111，第3位设置为0，结果是247
(check (copy-bit 4 255 #f) => 239)       ; 255 = #b11111111，第4位设置为0，结果是239
(check (copy-bit 5 255 #f) => 223)       ; 255 = #b11111111，第5位设置为0，结果是223
(check (copy-bit 6 255 #f) => 191)       ; 255 = #b11111111，第6位设置为0，结果是191
(check (copy-bit 7 255 #f) => 127)       ; 255 = #b11111111，第7位设置为0，结果是127

;;; 特殊值测试
(check (copy-bit 31 2147483647 #t) => 4294967295) ; 最大32位有符号整数，第31位设置为1，结果是4294967295
(check (copy-bit 31 -2147483648 #f) => -4294967296) ; 最小32位有符号整数，第31位设置为0，结果是-4294967296
(check (copy-bit 63 9223372036854775807 #t) => -1) ; 最大64位有符号整数，第63位设置为1，结果是-1
(check (copy-bit 63 -9223372036854775808 #f) => 0) ; 最小64位有符号整数，第63位设置为0，结果是0

;;; 负整数测试
(check (copy-bit 0 -2 #t) => -1)         ; -2 = #b11111110，第0位设置为1，结果是-1
(check (copy-bit 1 -1 #f) => -3)         ; -1 = #b11111111，第1位设置为0，结果是-3
(check (copy-bit 0 -3 #t) => -3)         ; -3 = #b11111101，第0位设置为1，结果不变
(check (copy-bit 1 -3 #f) => -3)         ; -3 = #b11111101，第1位设置为0，结果不变

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (copy-bit "string" 1 #t)   ; 索引参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (copy-bit 1 "string" #t)   ; 整数参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (copy-bit 3.14 2 #t)       ; 浮点数索引参数
) ;check-catch
(check-catch 'wrong-type-arg
             (copy-bit 1 3.14 #t)       ; 浮点数整数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (copy-bit #\a 1 #t)        ; 字符索引参数
) ;check-catch
(check-catch 'wrong-type-arg
             (copy-bit 1 #\a #t)        ; 字符整数参数
) ;check-catch

;;; 错误处理测试 - out-of-range
(check-catch 'out-of-range
             (copy-bit 64 -1 #f)        ; 索引不能超过63
) ;check-catch
(check-catch 'out-of-range
             (copy-bit 10000 -1 #f)     ; 索引不能超过63
) ;check-catch
(check-catch 'out-of-range
             (copy-bit -1 1 #t)         ; 索引不能为负数
) ;check-catch


(check-report)
