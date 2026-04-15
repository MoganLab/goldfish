(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bitwise-eqv
;; 计算两个整数的按位等价操作（XNOR）。
;;
;; 语法
;; ----
;; (bitwise-eqv i1 i2)
;;
;; 参数
;; ----
;; i1, i2 : integer?
;; 两个整数，参与按位等价操作。
;;
;; 返回值
;; -----
;; integer?
;; 返回两个整数按位等价操作的结果（整数）。
;;
;; 说明
;; ----
;; 1. 对两个整数的每一位进行等价操作（相同为1，不同为0）
;; 2. bitwise-eqv 等价于 (bitwise-not (bitwise-xor i1 i2))
;; 3. 对于任意整数 i，(bitwise-eqv i i) = -1 (所有位为1)
;; 4. 对于任意整数 i1 i2，(bitwise-eqv i1 i2) = (bitwise-eqv i2 i1)
;; 5. 按位等价操作满足交换律：(bitwise-eqv i1 i2) = (bitwise-eqv i2 i1)
;; 6. 按位等价操作常用于位模式的比较和验证
;;
;; 实现说明
;; --------
;; - bitwise-eqv 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 按位等价操作返回整数，不是布尔值
;; - 在逻辑上，bitwise-eqv 等价于 (bitwise-not (bitwise-xor i1 i2))
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。



;; ; 基本功能测试：按位等价操作（返回整数）
(check (bitwise-eqv 0 0) => -1)
(check (bitwise-eqv -1 -1) => -1)
(check (bitwise-eqv 0 -1) => 0)
(check (bitwise-eqv -1 0) => 0)


;; ; 与 bitwise-not/bitwise-xor 的关系测试
(check (bitwise-eqv 5 3)
  =>
  (bitwise-not (bitwise-xor 5 3))
) ;check
(check (bitwise-eqv 10 10)
  =>
  (bitwise-not (bitwise-xor 10 10))
) ;check
(check (bitwise-eqv 7 2)
  =>
  (bitwise-not (bitwise-xor 7 2))
) ;check
(check (bitwise-eqv 10 5)
  =>
  (bitwise-not (bitwise-xor 10 5))
) ;check


;; ; 数学性质测试
(check (bitwise-eqv 15 15) => -1)
(check (bitwise-eqv 7 3)
  =>
  (bitwise-eqv 3 7)
) ;check
(check (bitwise-eqv 255 255) => -1)


;; ; 二进制表示测试
(check (bitwise-eqv 10 10) => -1)
(check (bitwise-eqv 240 240) => -1)


;; ; 特殊值测试
(check (bitwise-eqv 2147483647 2147483647)
  =>
  -1
) ;check
(check (bitwise-eqv -2147483648 -2147483648)
  =>
  -1
) ;check
(check (bitwise-eqv 2147483647 -2147483648)
  =>
  (bitwise-not (bitwise-xor 2147483647 -2147483648)
  ) ;bitwise-not
) ;check


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (bitwise-eqv "string" 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-eqv 1 'symbol)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-eqv 3.14 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-eqv #\a 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bitwise-eqv '(1 2) 3)
) ;check-catch



(check-report)
