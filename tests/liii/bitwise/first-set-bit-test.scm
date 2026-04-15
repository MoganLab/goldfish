(import (liii check) (liii bitwise))


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



;; ; 基本功能测试：查找第一个设置位
(check (first-set-bit 1) => 0)
(check (first-set-bit 2) => 1)
(check (first-set-bit 0) => -1)
(check (first-set-bit 40) => 3)
(check (first-set-bit -28) => 2)
(check (first-set-bit (expt 2 62))
  =>
  62
) ;check
(check (first-set-bit (expt -2 62))
  =>
  62
) ;check


;; ; 边界值测试
(check (first-set-bit -1) => 0)
(check (first-set-bit 255) => 0)
(check (first-set-bit 256) => 8)
(check (first-set-bit -256) => 8)


;; ; 二进制表示测试
(check (first-set-bit 10) => 1)
(check (first-set-bit 5) => 0)
(check (first-set-bit 8) => 3)
(check (first-set-bit 1) => 0)
(check (first-set-bit 12) => 2)


;; ; 位模式测试
(check (first-set-bit 3) => 0)
(check (first-set-bit 4) => 2)
(check (first-set-bit 5) => 0)
(check (first-set-bit 6) => 1)
(check (first-set-bit 7) => 0)


;; ; 特殊值测试
(check (first-set-bit 2147483647) => 0)
(check (first-set-bit -2147483648)
  =>
  31
) ;check
(check (first-set-bit 9223372036854775807)
  =>
  0
) ;check
;; ; 注意：-9223372036854775808 超出范围，已注释掉
;; ; (check (first-set-bit -9223372036854775808) => 63) ; 最小64位有符号整数，第一个设置位是第63位


;; ; 负整数测试
(check (first-set-bit -2) => 1)
(check (first-set-bit -3) => 0)
(check (first-set-bit -4) => 2)
(check (first-set-bit -5) => 0)
(check (first-set-bit -6) => 1)


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (first-set-bit "string")
) ;check-catch
(check-catch 'wrong-type-arg
  (first-set-bit 'symbol)
) ;check-catch
(check-catch 'wrong-type-arg
  (first-set-bit 3.14)
) ;check-catch
(check-catch 'wrong-type-arg
  (first-set-bit #\a)
) ;check-catch
(check-catch 'wrong-type-arg
  (first-set-bit '(1 2))
) ;check-catch



(check-report)
