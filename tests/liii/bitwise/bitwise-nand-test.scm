(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bitwise-nand
;; 计算两个整数的按位与非操作。
;;
;; 语法
;; ----
;; (bitwise-nand i1 i2)
;;
;; 参数
;; ----
;; i1, i2 : integer?
;; 两个整数，参与按位与非操作。
;;
;; 返回值
;; -----
;; integer?
;; 返回两个整数按位与非操作的结果。
;;
;; 说明
;; ----
;; 1. 对两个整数的每一位进行与非操作（与操作后取反）
;; 2. 按位与非操作等价于 (bitwise-not (bitwise-and i1 i2))
;; 3. 对于任意整数 i1 i2，(bitwise-nand i1 i2) = (bitwise-nand i2 i1)
;; 4. 按位与非操作常用于逻辑电路设计和位掩码操作
;;
;; 实现说明
;; --------
;; - bitwise-nand 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 按位与非操作是与操作和取反操作的组合
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。



;; ; 精简测试：bitwise-nand 按位与非操作
(check (bitwise-nand 1 1) => -2)
(check (bitwise-nand 3 1) => -2)
(check (bitwise-nand 6 1) => -1)



(check-report)
