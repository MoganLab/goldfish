(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bitwise-ior
;; 计算多个整数的按位或操作。
;;
;; 语法
;; ----
;; (bitwise-ior i1 i2 ...)
;;
;; 参数
;; ----
;; i1, i2, ... : integer?
;; 一个或多个整数，参与按位或操作。
;;
;; 返回值
;; -----
;; integer?
;; 返回所有整数按位或操作的结果。
;;
;; 说明
;; ----
;; 1. 对所有整数的每一位进行或操作（任意一个为1时结果为1，否则为0）
;; 2. 按位或操作常用于设置特定位或合并位掩码
;; 3. 对于任意整数 i，(bitwise-ior i i) = i
;; 4. 对于任意整数 i，(bitwise-ior i 0) = i
;; 5. 对于任意整数 i，(bitwise-ior i -1) = -1
;; 6. 按位或操作满足交换律：(bitwise-ior i1 i2) = (bitwise-ior i2 i1)
;; 7. 按位或操作满足结合律：(bitwise-ior i1 (bitwise-ior i2 i3)) = (bitwise-ior (bitwise-ior i1 i2) i3)
;; 8. 支持两个或多个参数，按从左到右的顺序依次进行按位或操作
;;
;; 实现说明
;; --------
;; - bitwise-ior 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 如果考虑性能优化，可以使用 S7 Scheme 内置的 logior 函数
;; - logior 是 S7 的原生函数，通常比 bitwise-ior 有更好的性能
;;
;; 错误
;; ----
;; type-error
;; 当参数不是整数时抛出错误。



;; ; 基本功能测试：按位或操作
(check (bitwise-ior 5 3) => 7)
(check (bitwise-or 5 3) => 7)
(check (bitwise-ior 8 4) => 12)
(check (bitwise-ior 5 3) => 7)
(check (bitwise-ior 8 4) => 12)
(check (bitwise-ior 12 1) => 13)


;; ; 边界值测试
(check (bitwise-ior 0 0) => 0)
(check (bitwise-ior 0 1) => 1)
(check (bitwise-ior 1 0) => 1)
(check (bitwise-ior 1 1) => 1)
(check (bitwise-ior -1 -1) => -1)
(check (bitwise-ior -1 0) => -1)
(check (bitwise-ior 0 -1) => -1)


;; ; 数学性质测试
(check (bitwise-ior 15 15) => 15)
(check (bitwise-ior 7 3)
  =>
  (bitwise-ior 3 7)
) ;check
(check (bitwise-ior 15 (bitwise-ior 7 3))
  =>
  (bitwise-ior (bitwise-ior 15 7) 3)
) ;check
(check (bitwise-ior 255 0) => 255)
(check (bitwise-ior 255 -1) => -1)


;; ; 二进制表示测试
(check (bitwise-ior 170 85) => 255)
(check (bitwise-ior 240 204) => 252)
(check (bitwise-ior 15 240) => 255)


;; ; 三个参数测试
(check (bitwise-ior 1 2 4) => 7)
(check (bitwise-ior 1 1 1) => 1)
(check (bitwise-ior 0 1 2) => 3)
(check (bitwise-ior 5 3 7) => 7)


;; ; 特殊值测试
(check (bitwise-ior 2147483647 2147483647)
  =>
  2147483647
) ;check
(check (bitwise-ior -2147483648 -2147483648)
  =>
  -2147483648
) ;check
(check (bitwise-ior 2147483647 -2147483648)
  =>
  -1
) ;check
(check (bitwise-ior 4294967295 4294967295)
  =>
  4294967295
) ;check
(check (bitwise-ior 9223372036854775807
         9223372036854775807
       ) ;bitwise-ior
  =>
  9223372036854775807
) ;check
(check (bitwise-ior -9223372036854775808
         -9223372036854775808
       ) ;bitwise-ior
  =>
  -9223372036854775808
) ;check


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (bitwise-ior "string" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-ior 1 'symbol)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-ior 3.14 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-ior #\a 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-ior '(1 2) 3)
) ;check-catch


;; ; 多参数错误处理测试
(check-catch 'wrong-type-arg
  (bitwise-ior 1 2 3 "four")
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-ior 1 2 "three" 4)
) ;check-catch



(check-report)
