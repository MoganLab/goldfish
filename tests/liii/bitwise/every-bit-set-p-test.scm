(import (liii check) (liii bitwise))


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



;; ; 基本功能测试：检查位域中是否所有位设置
(check (every-bit-set? 3 7) => #t)
(check (every-bit-set? 3 6) => #f)


;; ; 边界值测试
(check (every-bit-set? 0 0) => #t)
(check (every-bit-set? 0 1) => #t)
(check (every-bit-set? 1 0) => #f)
(check (every-bit-set? 1 1) => #t)
(check (every-bit-set? -1 -1) => #t)
(check (every-bit-set? -1 0) => #f)
(check (every-bit-set? 0 -1) => #t)


;; ; 二进制表示测试
(check (every-bit-set? 10 10) => #t)
(check (every-bit-set? 10 14) => #t)
(check (every-bit-set? 10 2) => #f)
(check (every-bit-set? 15 0) => #f)
(check (every-bit-set? 15 15) => #t)
(check (every-bit-set? 8 8) => #t)
(check (every-bit-set? 8 7) => #f)


;; ; 位域测试：验证不同位域范围的检查
(check (every-bit-set? 12 12) => #t)
(check (every-bit-set? 12 14) => #t)
(check (every-bit-set? 12 10) => #f)
(check (every-bit-set? 3 3) => #t)
(check (every-bit-set? 3 7) => #t)
(check (every-bit-set? 3 5) => #f)


;; ; 特殊值测试
(check (every-bit-set? 2147483647 2147483647)
  =>
  #t
) ;check
(check (every-bit-set? 2147483647 0)
  =>
  #f
) ;check
(check (every-bit-set? -2147483648 -2147483648)
  =>
  #t
) ;check
(check (every-bit-set? -2147483648 0)
  =>
  #f
) ;check
(check (every-bit-set? 2147483647 -2147483648)
  =>
  #f
) ;check


;; ; 负整数测试
(check (every-bit-set? -1 -1) => #t)
(check (every-bit-set? -1 -2) => #f)
(check (every-bit-set? -2 -1) => #t)
(check (every-bit-set? -2 -3) => #f)
(check (every-bit-set? -1 0) => #f)
(check (every-bit-set? 0 -1) => #t)


;; ; 与 bitwise-and 的关系测试
(check (every-bit-set? 5 7)
  =>
  (= (bitwise-and 5 7) 5)
) ;check
(check (every-bit-set? 3 6)
  =>
  (= (bitwise-and 3 6) 3)
) ;check
(check (every-bit-set? 10 10)
  =>
  (= (bitwise-and 10 10) 10)
) ;check
(check (every-bit-set? 7 2)
  =>
  (= (bitwise-and 7 2) 7)
) ;check


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (every-bit-set? "string" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (every-bit-set? 1 "string")
) ;check-catch
(check-catch 'wrong-type-arg
  (every-bit-set? 3.14 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (every-bit-set? 1 3.14)
) ;check-catch
(check-catch 'wrong-type-arg
  (every-bit-set? #\a 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (every-bit-set? 1 #\a)
) ;check-catch
(check-catch 'wrong-type-arg
  (every-bit-set? '(1 2) 3)
) ;check-catch
(check-catch 'wrong-type-arg
  (every-bit-set? 1 '(2 3))
) ;check-catch



(check-report)
