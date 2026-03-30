(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; first-set-bit
;; 查找整数中第一个被设置的位（值为1的位）的位置。
;;
;; 语法
;; ----
;; (first-set-bit i)
;;
;; 参数
;; ----
;; i : integer?
;; 整数，要查找第一个设置位的整数。
;;
;; 返回值
;; -----
;; integer?
;; 返回整数 i 中第一个被设置的位（值为1的位）的位置，从0开始计数。
;; 如果整数为0（没有设置任何位），返回-1。
;;
;; 说明
;; ----
;; 1. 查找整数二进制表示中第一个值为1的位的位置
;; 2. 位位置从0开始计数，0表示最低有效位（LSB）
;; 3. 对于非负整数，查找第一个值为1的位
;; 4. 对于负整数，查找第一个值为1的位（在补码表示中）
;; 5. 如果整数为0，返回-1，表示没有设置任何位
;; 6. 常用于位扫描、查找最低有效设置位等场景
;;
;; 实现说明
;; --------
;; - first-set-bit 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用 S7 Scheme 内置的位运算函数实现
;; - 支持所有整数类型，包括负整数
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。


;;; 基本功能测试：查找第一个设置位
(check (first-set-bit 1) => 0)
(check (first-set-bit 2) => 1)
(check (first-set-bit 0) => -1)
(check (first-set-bit 40) => 3)
(check (first-set-bit -28) => 2)
(check (first-set-bit (expt  2 62)) => 62)
(check (first-set-bit (expt -2 62)) => 62)

;;; 边界值测试
(check (first-set-bit -1) => 0)          ; -1的所有位都是1，第一个设置位是第0位
(check (first-set-bit 255) => 0)         ; 255 = #b11111111，第一个设置位是第0位
(check (first-set-bit 256) => 8)         ; 256 = #b100000000，第一个设置位是第8位
(check (first-set-bit -256) => 8)        ; -256 = #b11111111111111111111111100000000，第一个设置位是第8位

;;; 二进制表示测试
(check (first-set-bit #b1010) => 1)      ; #b1010 第一个设置位是第1位
(check (first-set-bit #b0101) => 0)      ; #b0101 第一个设置位是第0位
(check (first-set-bit #b1000) => 3)      ; #b1000 第一个设置位是第3位
(check (first-set-bit #b0001) => 0)      ; #b0001 第一个设置位是第0位
(check (first-set-bit #b1100) => 2)      ; #b1100 第一个设置位是第2位

;;; 位模式测试
(check (first-set-bit 3) => 0)           ; 3 = #b11，第一个设置位是第0位
(check (first-set-bit 4) => 2)           ; 4 = #b100，第一个设置位是第2位
(check (first-set-bit 5) => 0)           ; 5 = #b101，第一个设置位是第0位
(check (first-set-bit 6) => 1)           ; 6 = #b110，第一个设置位是第1位
(check (first-set-bit 7) => 0)           ; 7 = #b111，第一个设置位是第0位

;;; 特殊值测试
(check (first-set-bit 2147483647) => 0)  ; 最大32位有符号整数，第一个设置位是第0位
(check (first-set-bit -2147483648) => 31) ; 最小32位有符号整数，第一个设置位是第31位
(check (first-set-bit 9223372036854775807) => 0)  ; 最大64位有符号整数，第一个设置位是第0位
;;; 注意：-9223372036854775808 超出范围，已注释掉
;;; (check (first-set-bit -9223372036854775808) => 63) ; 最小64位有符号整数，第一个设置位是第63位

;;; 负整数测试
(check (first-set-bit -2) => 1)          ; -2 = #b11111110，第一个设置位是第1位
(check (first-set-bit -3) => 0)          ; -3 = #b11111101，第一个设置位是第0位
(check (first-set-bit -4) => 2)          ; -4 = #b11111100，第一个设置位是第2位
(check (first-set-bit -5) => 0)          ; -5 = #b11111011，第一个设置位是第0位
(check (first-set-bit -6) => 1)          ; -6 = #b11111010，第一个设置位是第1位

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (first-set-bit "string")   ; 字符串参数
) ;check-catch
(check-catch 'wrong-type-arg
             (first-set-bit 'symbol)    ; 符号参数
) ;check-catch
(check-catch 'wrong-type-arg
             (first-set-bit 3.14)       ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (first-set-bit #\a)        ; 字符参数
) ;check-catch
(check-catch 'wrong-type-arg
             (first-set-bit '(1 2))     ; 列表参数
) ;check-catch


(check-report)
