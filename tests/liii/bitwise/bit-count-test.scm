(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bit-count
;; 计算整数中值为1的位数。
;;
;; 语法
;; ----
;; (bit-count i)
;;
;; 参数
;; ----
;; i : integer?
;; 整数，要计算值为1的位数的整数。
;;
;; 返回值
;; -----
;; integer?
;; 返回整数 i 中值为1的位数。
;;
;; 说明
;; ----
;; 1. 计算整数二进制表示中值为1的位数
;; 2. 对于非负整数，返回值为1的位数
;; 3. 对于负整数，返回值为0的位数
;; 4. 常用于计算汉明权重或位密度
;;
;; 实现说明
;; --------
;; - bit-count 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 对于非负整数，计算值为1的位数
;; - 对于负整数，计算值为0的位数
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。



;; ; 精简测试：bit-count 位数计算
(check (bit-count 0) => 0)
(check (bit-count -1) => 0)
(check (bit-count 7) => 3)
(check (bit-count 13) => 3)
(check (bit-count -13) => 2)



(check-report)
