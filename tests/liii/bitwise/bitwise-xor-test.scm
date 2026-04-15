(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bitwise-xor
;; 计算多个整数的按位异或操作。
;;
;; 语法
;; ----
;; (bitwise-xor i1 i2 ...)
;;
;; 参数
;; ----
;; i1, i2, ... : integer?
;; 一个或多个整数，参与按位异或操作。
;;
;; 返回值
;; -----
;; integer?
;; 返回所有整数按位异或操作的结果。
;;
;; 说明
;; ----
;; 1. 对所有整数的每一位进行异或操作（相同为0，不同为1）
;; 2. 按位异或操作常用于比较位差异或实现简单的加密
;; 3. 对于任意整数 i，(bitwise-xor i i) = 0
;; 4. 对于任意整数 i，(bitwise-xor i 0) = i
;; 5. 对于任意整数 i，(bitwise-xor i -1) = (bitwise-not i)
;; 6. 按位异或操作满足交换律：(bitwise-xor i1 i2) = (bitwise-xor i2 i1)
;; 7. 按位异或操作满足结合律：(bitwise-xor i1 (bitwise-xor i2 i3)) = (bitwise-xor (bitwise-xor i1 i2) i3)
;; 8. 支持两个或多个参数，按从左到右的顺序依次进行按位异或操作
;;
;; 实现说明
;; --------
;; - bitwise-xor 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 如果考虑性能优化，可以使用 S7 Scheme 内置的 logxor 函数
;; - logxor 是 S7 的原生函数，通常比 bitwise-xor 有更好的性能
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。



;; ; 基本功能测试：按位异或操作
(check (bitwise-xor 1 1) => 0)
(check (bitwise-xor 2 3) => 1)
(check (bitwise-xor 42 52) => 30)
(check (bitwise-xor 0 0) => 0)
(check (bitwise-xor 1 1) => 0)
(check (bitwise-xor 5 7) => 2)
(check (bitwise-xor 8 9) => 1)
(check (bitwise-xor 149 121) => 236)


;; ; 边界值测试
(check (bitwise-xor 0 0) => 0)
(check (bitwise-xor 0 1) => 1)
(check (bitwise-xor 1 0) => 1)
(check (bitwise-xor 1 1) => 0)
(check (bitwise-xor -1 -1) => 0)
(check (bitwise-xor -1 0) => -1)
(check (bitwise-xor 0 -1) => -1)


;; ; 数学性质测试
(check (bitwise-xor 15 15) => 0)
(check (bitwise-xor 7 3)
  =>
  (bitwise-xor 3 7)
) ;check
(check (bitwise-xor 15 (bitwise-xor 7 3))
  =>
  (bitwise-xor (bitwise-xor 15 7) 3)
) ;check
(check (bitwise-xor 255 0) => 255)
(check (bitwise-xor 255 -1) => -256)


;; ; 二进制表示测试
(check (bitwise-xor 170 85) => 255)
(check (bitwise-xor 240 204) => 60)
(check (bitwise-xor 15 240) => 255)


;; ; 三个参数测试
(check (bitwise-xor 1 2 4) => 7)
(check (bitwise-xor 1 1 1) => 1)
(check (bitwise-xor 0 1 2) => 3)
(check (bitwise-xor 5 3 7) => 1)


;; ; 特殊值测试
(check (bitwise-xor 2147483647 2147483647)
  =>
  0
) ;check
(check (bitwise-xor -2147483648 -2147483648)
  =>
  0
) ;check
(check (bitwise-xor 2147483647 -2147483648)
  =>
  -1
) ;check


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (bitwise-xor "string" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-xor 1 'symbol)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-xor 3.14 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-xor #\a 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-xor '(1 2) 3)
) ;check-catch


;; ; 多参数错误处理测试
(check-catch 'wrong-type-arg
  (bitwise-xor 1 2 3 "four")
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-xor 1 2 "three" 4)
) ;check-catch



(check-report)
