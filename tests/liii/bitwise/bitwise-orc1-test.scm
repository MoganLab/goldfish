(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bitwise-orc1
;; 计算两个整数的按位或非操作（第一个参数取反）。
;;
;; 语法
;; ----
;; (bitwise-orc1 i1 i2)
;;
;; 参数
;; ----
;; i1, i2 : integer?
;; 两个整数，参与按位或非操作。
;;
;; 返回值
;; -----
;; integer?
;; 返回两个整数按位或非操作的结果。
;;
;; 说明
;; ----
;; 1. 对两个整数的每一位进行或非操作（第一个参数取反后与第二个参数进行或操作）
;; 2. 按位或非操作等价于 (bitwise-ior (bitwise-not i1) i2)
;; 3. 对于任意整数 i1 i2，(bitwise-orc1 i1 i2) = (bitwise-orc1 i2 i1)
;; 4. 按位或非操作常用于逻辑电路设计和位掩码操作
;;
;; 实现说明
;; --------
;; - bitwise-orc1 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 按位或非操作是取反操作和或操作的组合
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。



;; ; 精简测试：bitwise-orc1 按位或非操作
(check (bitwise-orc1 1 1) => -1)
(check (bitwise-orc1 3 1) => -3)
(check (bitwise-orc1 11 26) => -2)
(check (bitwise-orc1 6 1) => -7)



(check-report)
