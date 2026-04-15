(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bitwise-or
;; 计算多个整数的按位或操作。
;;
;; 语法
;; ----
;; (bitwise-or i1 i2 ...)
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
;; 2. bitwise-or 是 bitwise-ior 的别名，两者功能完全相同
;; 3. 支持两个或多个参数，按从左到右的顺序依次进行按位或操作
;;
;; 实现说明
;; --------
;; - bitwise-or 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 作为 bitwise-ior 的别名，提供更简洁的函数名
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。



;; ; 精简测试：bitwise-or 作为 bitwise-ior 的别名
(check (bitwise-or 5 3) => 7)
(check (bitwise-or 8 4) => 12)
(check (bitwise-or 1 2 4) => 7)


;; ; 验证 bitwise-or 与 bitwise-ior 功能相同
(check (bitwise-or 5 3)
  =>
  (bitwise-ior 5 3)
) ;check
(check (bitwise-or 8 4)
  =>
  (bitwise-ior 8 4)
) ;check
(check (bitwise-or 1 2 4)
  =>
  (bitwise-ior 1 2 4)
) ;check



(check-report)
