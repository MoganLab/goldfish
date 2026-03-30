(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; any-bit-set?
;; 检查位域中是否有任何位被设置（值为1）。
;;
;; 语法
;; ----
;; (any-bit-set? test-bits n)
;;
;; 参数
;; ----
;; test-bits : integer?
;; 位域掩码，指定要检查的位位置。
;; n : integer?
;; 整数，要检查位设置的整数。
;;
;; 返回值
;; -----
;; boolean?
;; 如果整数 n 中由 test-bits 指定的位域中有任何位被设置（值为1），返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 检查整数 n 中由 test-bits 指定的位域中是否有任何位被设置
;; 2. test-bits 是一个位掩码，其中值为1的位表示要检查的位置
;; 3. 当且仅当 (bitwise-and test-bits n) ≠ 0 时返回 #t
;; 4. 常用于检查一组标志位中是否有任何标志被设置
;; 5. 与 every-bit-set? 函数互补，any-bit-set? 检查是否有任何位设置，而 every-bit-set? 检查是否所有位都设置
;;
;; 实现说明
;; --------
;; - any-bit-set? 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用 bitwise-and 操作实现，检查按位与结果是否非零
;; - 支持所有整数类型，包括负整数
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。


;;; 基本功能测试：检查位域中是否有任何位设置
(check (any-bit-set? 3 6) => #t)  ; 3 = #b0011, 6 = #b0110, 按位与 = #b0010 ≠ 0
(check (any-bit-set? 3 12) => #f) ; 3 = #b0011, 12 = #b1100, 按位与 = #b0000 = 0

;;; 边界值测试
(check (any-bit-set? 0 0) => #f)          ; 0 AND 0 = 0，没有位设置
(check (any-bit-set? 0 1) => #f)          ; 0 AND 1 = 0，没有位设置
(check (any-bit-set? 1 0) => #f)          ; 1 AND 0 = 0，没有位设置
(check (any-bit-set? 1 1) => #t)          ; 1 AND 1 = 1，有位设置
(check (any-bit-set? -1 -1) => #t)        ; -1 AND -1 = -1，所有位都设置
(check (any-bit-set? -1 0) => #f)         ; -1 AND 0 = 0，没有位设置
(check (any-bit-set? 0 -1) => #f)         ; 0 AND -1 = 0，没有位设置

;;; 二进制表示测试
(check (any-bit-set? #b1010 #b0101) => #f) ; #b1010 AND #b0101 = #b0000 = 0，没有位设置
(check (any-bit-set? #b1010 #b0110) => #t) ; #b1010 AND #b0110 = #b0010 ≠ 0，有位设置
(check (any-bit-set? #b1111 #b0000) => #f) ; #b1111 AND #b0000 = #b0000 = 0，没有位设置
(check (any-bit-set? #b1111 #b1111) => #t) ; #b1111 AND #b1111 = #b1111 ≠ 0，所有位都设置
(check (any-bit-set? #b1000 #b1000) => #t) ; #b1000 AND #b1000 = #b1000 ≠ 0，有位设置
(check (any-bit-set? #b1000 #b0111) => #f) ; #b1000 AND #b0111 = #b0000 = 0，没有位设置

;;; 位域测试：验证不同位域范围的检查
(check (any-bit-set? #b1100 #b0011) => #f) ; #b1100 AND #b0011 = #b0000 = 0，没有位设置
(check (any-bit-set? #b1100 #b1010) => #t) ; #b1100 AND #b1010 = #b1000 ≠ 0，有位设置
(check (any-bit-set? #b0011 #b1100) => #f) ; #b0011 AND #b1100 = #b0000 = 0，没有位设置
(check (any-bit-set? #b0011 #b0110) => #t) ; #b0011 AND #b0110 = #b0010 ≠ 0，有位设置

;;; 特殊值测试
(check (any-bit-set? 2147483647 2147483647) => #t) ; 最大32位有符号整数，所有位都设置
(check (any-bit-set? 2147483647 0) => #f)          ; 最大32位有符号整数 AND 0 = 0，没有位设置
(check (any-bit-set? -2147483648 -2147483648) => #t) ; 最小32位有符号整数，所有位都设置
(check (any-bit-set? -2147483648 0) => #f)         ; 最小32位有符号整数 AND 0 = 0，没有位设置
(check (any-bit-set? 2147483647 -2147483648) => #f) ; 最大和最小整数，没有共同的设置位

;;; 负整数测试
(check (any-bit-set? -1 -1) => #t)         ; -1 AND -1 = -1，所有位都设置
(check (any-bit-set? -1 -2) => #t)         ; -1 AND -2 = -2 ≠ 0，有位设置
(check (any-bit-set? -2 -1) => #t)         ; -2 AND -1 = -2 ≠ 0，有位设置
(check (any-bit-set? -2 -3) => #t)         ; -2 AND -3 = -4 ≠ 0，有位设置
(check (any-bit-set? -1 0) => #f)          ; -1 AND 0 = 0，没有位设置
(check (any-bit-set? 0 -1) => #f)          ; 0 AND -1 = 0，没有位设置

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (any-bit-set? "string" 1)   ; test-bits 参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (any-bit-set? 1 "string")   ; n 参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (any-bit-set? 3.14 2)       ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (any-bit-set? 1 3.14)       ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (any-bit-set? #\a 1)        ; 字符参数
) ;check-catch
(check-catch 'wrong-type-arg
             (any-bit-set? 1 #\a)        ; 字符参数
) ;check-catch
(check-catch 'wrong-type-arg
             (any-bit-set? '(1 2) 3)     ; 列表参数
) ;check-catch
(check-catch 'wrong-type-arg
             (any-bit-set? 1 '(2 3))     ; 列表参数
) ;check-catch


(check-report)
