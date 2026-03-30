(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; bitwise-andc2
;; 计算两个整数的按位与非操作（第二个参数取反）。
;;
;; 语法
;; ----
;; (bitwise-andc2 i1 i2)
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
;; 1. 对两个整数的每一位进行与非操作（第一个参数与第二个参数取反后进行与操作）
;; 2. 按位与非操作等价于 (bitwise-and i1 (bitwise-not i2))
;; 3. 按位与非操作常用于逻辑电路设计和位掩码操作
;;
;; 实现说明
;; --------
;; - bitwise-andc2 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 按位与非操作是与操作和取反操作的组合
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。

;;; 精简测试：bitwise-andc2 按位与非操作
(check (bitwise-andc2 11 26) => 1)
(check (bitwise-andc2 5 3) => 4)
(check (bitwise-andc2 #b1100 #b1010) => 4)
(check (bitwise-andc2 0 15) => 0)
(check (bitwise-andc2 15 0) => 15)
(check (bitwise-andc2 7 1) => 6)


(check-report)
