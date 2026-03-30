(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; bitwise-andc1
;; 计算两个整数的按位与非操作（第一个参数取反）。
;;
;; 语法
;; ----
;; (bitwise-andc1 i1 i2)
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
;; 1. 对两个整数的每一位进行与非操作（第一个参数取反后与第二个参数进行与操作）
;; 2. 按位与非操作等价于 (bitwise-and (bitwise-not i1) i2)
;; 3. 对于任意整数 i1 i2，(bitwise-andc1 i1 i2) = (bitwise-andc1 i2 i1)
;; 4. 按位与非操作常用于逻辑电路设计和位掩码操作
;;
;; 实现说明
;; --------
;; - bitwise-andc1 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 按位与非操作是取反操作和与操作的组合
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。

;;; 精简测试：bitwise-andc1 按位与非操作
(check (bitwise-andc1 11 26) => 16)
(check (bitwise-andc1 5 3) => 2)
(check (bitwise-andc1 #b1100 #b1010) => 2)
(check (bitwise-andc1 0 15) => 15)


(check-report)
