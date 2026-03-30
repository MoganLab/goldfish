(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; every-bit-set?
;; 检查位域中是否所有位都被设置（值为1）。
;;
;; 语法
;; ----
;; (every-bit-set? test-bits n)
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
;; 如果整数 n 中由 test-bits 指定的位域中所有位都被设置（值为1），返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 检查整数 n 中由 test-bits 指定的位域中是否所有位都被设置
;; 2. test-bits 是一个位掩码，其中值为1的位表示要检查的位置
;; 3. 当且仅当 (bitwise-and test-bits n) = test-bits 时返回 #t
;; 4. 常用于验证一组标志位是否全部被设置
;; 5. 与 any-bit-set? 函数互补，every-bit-set? 检查是否所有位都设置，而 any-bit-set? 检查是否有任何位设置
;; 6. 对于空位域（test-bits = 0），总是返回 #t，因为没有位需要检查
;;
;; 实现说明
;; --------
;; - every-bit-set? 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用 bitwise-and 操作实现，检查按位与结果是否等于 test-bits
;; - 支持所有整数类型，包括负整数
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。


;;; 基本功能测试：检查位域中是否所有位设置
(check (every-bit-set? 3 7) => #t)  ; 3 = #b0011, 7 = #b0111, 按位与 = #b0011 = 3
(check (every-bit-set? 3 6) => #f)  ; 3 = #b0011, 6 = #b0110, 按位与 = #b0010 ≠ 3

;;; 边界值测试
(check (every-bit-set? 0 0) => #t)          ; 空位域，没有位需要检查
(check (every-bit-set? 0 1) => #t)          ; 空位域，没有位需要检查
(check (every-bit-set? 1 0) => #f)          ; 1 AND 0 = 0 ≠ 1，不是所有位设置
(check (every-bit-set? 1 1) => #t)          ; 1 AND 1 = 1 = 1，所有位设置
(check (every-bit-set? -1 -1) => #t)        ; -1 AND -1 = -1 = -1，所有位设置
(check (every-bit-set? -1 0) => #f)         ; -1 AND 0 = 0 ≠ -1，不是所有位设置
(check (every-bit-set? 0 -1) => #t)         ; 空位域，没有位需要检查

;;; 二进制表示测试
(check (every-bit-set? #b1010 #b1010) => #t) ; #b1010 AND #b1010 = #b1010 = #b1010，所有位设置
(check (every-bit-set? #b1010 #b1110) => #t) ; #b1010 AND #b1110 = #b1010 = #b1010，所有位设置
(check (every-bit-set? #b1010 #b0010) => #f) ; #b1010 AND #b0010 = #b0010 ≠ #b1010，不是所有位设置
(check (every-bit-set? #b1111 #b0000) => #f) ; #b1111 AND #b0000 = #b0000 ≠ #b1111，不是所有位设置
(check (every-bit-set? #b1111 #b1111) => #t) ; #b1111 AND #b1111 = #b1111 = #b1111，所有位设置
(check (every-bit-set? #b1000 #b1000) => #t) ; #b1000 AND #b1000 = #b1000 = #b1000，所有位设置
(check (every-bit-set? #b1000 #b0111) => #f) ; #b1000 AND #b0111 = #b0000 ≠ #b1000，不是所有位设置

;;; 位域测试：验证不同位域范围的检查
(check (every-bit-set? #b1100 #b1100) => #t) ; #b1100 AND #b1100 = #b1100 = #b1100，所有位设置
(check (every-bit-set? #b1100 #b1110) => #t) ; #b1100 AND #b1110 = #b1100 = #b1100，所有位设置
(check (every-bit-set? #b1100 #b1010) => #f) ; #b1100 AND #b1010 = #b1000 ≠ #b1100，不是所有位设置
(check (every-bit-set? #b0011 #b0011) => #t) ; #b0011 AND #b0011 = #b0011 = #b0011，所有位设置
(check (every-bit-set? #b0011 #b0111) => #t) ; #b0011 AND #b0111 = #b0011 = #b0011，所有位设置
(check (every-bit-set? #b0011 #b0101) => #f) ; #b0011 AND #b0101 = #b0001 ≠ #b0011，不是所有位设置

;;; 特殊值测试
(check (every-bit-set? 2147483647 2147483647) => #t) ; 最大32位有符号整数，所有位都设置
(check (every-bit-set? 2147483647 0) => #f)          ; 最大32位有符号整数 AND 0 = 0 ≠ 2147483647，不是所有位设置
(check (every-bit-set? -2147483648 -2147483648) => #t) ; 最小32位有符号整数，所有位都设置
(check (every-bit-set? -2147483648 0) => #f)         ; 最小32位有符号整数 AND 0 = 0 ≠ -2147483648，不是所有位设置
(check (every-bit-set? 2147483647 -2147483648) => #f) ; 最大和最小整数，没有共同的设置位

;;; 负整数测试
(check (every-bit-set? -1 -1) => #t)         ; -1 AND -1 = -1 = -1，所有位都设置
(check (every-bit-set? -1 -2) => #f)         ; -1 AND -2 = -2 ≠ -1，不是所有位设置
(check (every-bit-set? -2 -1) => #t)         ; -2 AND -1 = -2 = -2，所有位都设置
(check (every-bit-set? -2 -3) => #f)         ; -2 AND -3 = -4 ≠ -2，不是所有位设置
(check (every-bit-set? -1 0) => #f)          ; -1 AND 0 = 0 ≠ -1，不是所有位设置
(check (every-bit-set? 0 -1) => #t)          ; 空位域，没有位需要检查

;;; 与 bitwise-and 的关系测试
(check (every-bit-set? 5 7) => (= (bitwise-and 5 7) 5)) ; 等价性验证
(check (every-bit-set? 3 6) => (= (bitwise-and 3 6) 3)) ; 等价性验证
(check (every-bit-set? 10 10) => (= (bitwise-and 10 10) 10)) ; 相同数
(check (every-bit-set? 7 2) => (= (bitwise-and 7 2) 7)) ; 不同数

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (every-bit-set? "string" 1)   ; test-bits 参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (every-bit-set? 1 "string")   ; n 参数不是整数
) ;check-catch
(check-catch 'wrong-type-arg
             (every-bit-set? 3.14 2)       ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (every-bit-set? 1 3.14)       ; 浮点数参数
) ;check-catch
(check-catch 'wrong-type-arg
             (every-bit-set? #\a 1)        ; 字符参数
) ;check-catch
(check-catch 'wrong-type-arg
             (every-bit-set? 1 #\a)        ; 字符参数
) ;check-catch
(check-catch 'wrong-type-arg
             (every-bit-set? '(1 2) 3)     ; 列表参数
) ;check-catch
(check-catch 'wrong-type-arg
             (every-bit-set? 1 '(2 3))     ; 列表参数
) ;check-catch


(check-report)
