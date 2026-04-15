(import (liii check) (liii bitwise))


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



;; ; 基本功能测试：检查位域中是否有任何位设置
(check (any-bit-set? 3 6) => #t)
(check (any-bit-set? 3 12) => #f)


;; ; 边界值测试
(check (any-bit-set? 0 0) => #f)
(check (any-bit-set? 0 1) => #f)
(check (any-bit-set? 1 0) => #f)
(check (any-bit-set? 1 1) => #t)
(check (any-bit-set? -1 -1) => #t)
(check (any-bit-set? -1 0) => #f)
(check (any-bit-set? 0 -1) => #f)


;; ; 二进制表示测试
(check (any-bit-set? 10 5) => #f)
(check (any-bit-set? 10 6) => #t)
(check (any-bit-set? 15 0) => #f)
(check (any-bit-set? 15 15) => #t)
(check (any-bit-set? 8 8) => #t)
(check (any-bit-set? 8 7) => #f)


;; ; 位域测试：验证不同位域范围的检查
(check (any-bit-set? 12 3) => #f)
(check (any-bit-set? 12 10) => #t)
(check (any-bit-set? 3 12) => #f)
(check (any-bit-set? 3 6) => #t)


;; ; 特殊值测试
(check (any-bit-set? 2147483647 2147483647)
  =>
  #t
) ;check
(check (any-bit-set? 2147483647 0)
  =>
  #f
) ;check
(check (any-bit-set? -2147483648 -2147483648)
  =>
  #t
) ;check
(check (any-bit-set? -2147483648 0)
  =>
  #f
) ;check
(check (any-bit-set? 2147483647 -2147483648)
  =>
  #f
) ;check


;; ; 负整数测试
(check (any-bit-set? -1 -1) => #t)
(check (any-bit-set? -1 -2) => #t)
(check (any-bit-set? -2 -1) => #t)
(check (any-bit-set? -2 -3) => #t)
(check (any-bit-set? -1 0) => #f)
(check (any-bit-set? 0 -1) => #f)


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (any-bit-set? "string" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (any-bit-set? 1 "string")
) ;check-catch
(check-catch 'wrong-type-arg
  (any-bit-set? 3.14 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (any-bit-set? 1 3.14)
) ;check-catch
(check-catch 'wrong-type-arg
  (any-bit-set? #\a 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (any-bit-set? 1 #\a)
) ;check-catch
(check-catch 'wrong-type-arg
  (any-bit-set? '(1 2) 3)
) ;check-catch
(check-catch 'wrong-type-arg
  (any-bit-set? 1 '(2 3))
) ;check-catch



(check-report)
