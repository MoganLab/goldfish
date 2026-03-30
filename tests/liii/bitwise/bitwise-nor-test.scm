(import (liii check)
        (liii bitwise)
) ;import

(check-set-mode! 'report-failed)

;; bitwise-nor
;; 计算两个整数的按位或非操作。
;;
;; 语法
;; ----
;; (bitwise-nor i1 i2)
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
;; 1. 对两个整数的每一位进行或非操作（或操作后取反）
;; 2. 按位或非操作等价于 (bitwise-not (bitwise-ior i1 i2))
;; 3. 对于任意整数 i1 i2，(bitwise-nor i1 i2) = (bitwise-nor i2 i1)
;; 4. 按位或非操作常用于逻辑电路设计和位掩码操作
;;
;; 实现说明
;; --------
;; - bitwise-nor 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 按位或非操作是或操作和取反操作的组合
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。


;;; 精简测试：bitwise-nor 按位或非操作
(check (bitwise-nor 2 4) => -7)  ; 2 (010) NOR 4 (100) = -7 (11111001)
(check (bitwise-nor 3 1) => -4)  ; 3 (011) NOR 1 (001) = -4 (11111100)
(check (bitwise-nor #b111 #b011) => -8)  ; 7 (111) NOR 3 (011) = -8 (11111000)


(check-report)
