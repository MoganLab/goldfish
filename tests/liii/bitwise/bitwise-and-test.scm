(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bitwise-and
;; 计算多个整数的按位与操作。
;;
;; 语法
;; ----
;; (bitwise-and i1 i2 ...)
;;
;; 参数
;; ----
;; i1, i2, ... : integer?
;; 一个或多个整数，参与按位与操作。
;;
;; 返回值
;; -----
;; integer?
;; 返回所有整数按位与操作的结果。
;;
;; 说明
;; ----
;; 1. 对所有整数的每一位进行与操作（都为1时结果为1，否则为0）
;; 2. 按位与操作常用于提取特定位或掩码操作
;; 3. 对于任意整数 i，(bitwise-and i i) = i
;; 4. 对于任意整数 i，(bitwise-and i 0) = 0
;; 5. 对于任意整数 i，(bitwise-and i -1) = i
;; 6. 按位与操作满足交换律：(bitwise-and i1 i2) = (bitwise-and i2 i1)
;; 7. 按位与操作满足结合律：(bitwise-and i1 (bitwise-and i2 i3)) = (bitwise-and (bitwise-and i1 i2) i3)
;; 8. 支持两个或多个参数，按从左到右的顺序依次进行按位与操作
;;
;; 实现说明
;; --------
;; - bitwise-and 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 在 Goldfish Scheme 中，bitwise-and 直接定义为 logand 的别名
;; - logand 是 S7 的原生函数，支持多个参数的按位与操作
;; - 使用 S7 内置的 logand 函数提供更好的性能和兼容性
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。



;; ; 基本功能测试：按位与操作
(check (bitwise-and 5 3) => 1)
(check (bitwise-and 8 4) => 0)
(check (bitwise-and 5 3) => 1)
(check (bitwise-and 8 4) => 0)
(check (bitwise-and 12 10) => 8)


;; ; 边界值测试
(check (bitwise-and 0 0) => 0)
(check (bitwise-and 0 1) => 0)
(check (bitwise-and 1 0) => 0)
(check (bitwise-and 1 1) => 1)
(check (bitwise-and -1 -1) => -1)
(check (bitwise-and -1 0) => 0)
(check (bitwise-and 0 -1) => 0)


;; ; 数学性质测试
(check (bitwise-and 15 15) => 15)
(check (bitwise-and 7 3)
  =>
  (bitwise-and 3 7)
) ;check
(check (bitwise-and 15 (bitwise-and 7 3))
  =>
  (bitwise-and (bitwise-and 15 7) 3)
) ;check
(check (bitwise-and 255 0) => 0)
(check (bitwise-and 255 -1) => 255)


;; ; 二进制表示测试
(check (bitwise-and 170 85) => 0)
(check (bitwise-and 240 204) => 192)
(check (bitwise-and 255 15) => 15)
(check (bitwise-and 255 240) => 240)


;; ; 特殊值测试
(check (bitwise-and 2147483647 2147483647)
  =>
  2147483647
) ;check
(check (bitwise-and -2147483648 -2147483648)
  =>
  -2147483648
) ;check
(check (bitwise-and 2147483647 -2147483648)
  =>
  0
) ;check


;; ; 三个参数测试
(check (bitwise-and 1 2 3) => 0)
(check (bitwise-and 7 3 5) => 1)
(check (bitwise-and 15 7 3) => 3)
(check (bitwise-and 5 3 7) => 1)
(check (bitwise-and 12 10 6) => 0)
(check (bitwise-and 255 127 63) => 63)
(check (bitwise-and -1 -1 -1) => -1)
(check (bitwise-and 0 1 2) => 0)
(check (bitwise-and 1 1 1) => 1)
(check (bitwise-and 2 2 2) => 2)


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (bitwise-and "string" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-and 1 'symbol)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-and 3.14 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-and #\a 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-and '(1 2) 3)
) ;check-catch


;; ; 多参数错误处理测试
(check-catch 'wrong-type-arg
  (bitwise-and 1 2 3 "four")
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-and 1 2 "three" 4)
) ;check-catch



(check-report)
