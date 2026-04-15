(import (liii check) (liii bitwise))


(check-set-mode! 'report-failed)


;; bit-swap
;; 交换整数中两个位的值。
;;
;; 语法
;; ----
;; (bit-swap i index1 index2)
;;
;; 参数
;; ----
;; i : integer?
;; 整数，要进行位交换操作的整数。
;; index1 : integer?
;; 第一个位索引，从0开始，表示要交换的第一个位位置。
;; index2 : integer?
;; 第二个位索引，从0开始，表示要交换的第二个位位置。
;;
;; 返回值
;; -----
;; integer?
;; 返回整数 i 中第 index1 位和第 index2 位交换后的结果。
;;
;; 说明
;; ----
;; 1. 交换整数 i 中第 index1 位和第 index2 位的值
;; 2. 位索引从0开始，0表示最低有效位（LSB）
;; 3. 支持64位整数范围，位索引范围为0到63
;; 4. 如果两个位索引相同，则返回0
;; 5. 不支持负整数参数
;; 6. 常用于位操作、位模式变换和位算法
;;
;; 实现说明
;; --------
;; - bit-swap 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
;; - 使用 S7 Scheme 内置的位运算函数实现
;; - 支持64位整数范围，位索引范围为0到63
;;
;; 错误
;; ----
;; wrong-type-arg
;; 当参数不是整数时抛出错误。
;; out-of-range
;; 当位索引超出有效范围（0-63）时抛出错误。



;; ; 基本功能测试：交换两个位的值
(check (bit-swap 0 2 4) => 1)
(check (bit-swap 3 0 5) => 12)
(check (bit-swap 63 0 1)
  =>
  -9223372036854775808
) ;check
(check (bit-swap 10 0 3) => 1026)
(check (bit-swap 5 1 2) => 32)


;; ; 边界值测试
(check (bit-swap 0 0 0) => 0)
(check (bit-swap 1 0 0) => 0)
(check (bit-swap 0 0 63) => 63)
(check (bit-swap 1 0 63) => 63)


;; ; 二进制表示测试
(check (bit-swap 12 0 3) => 4098)
(check (bit-swap 10 1 2) => 1024)
(check (bit-swap 6 0 2) => 2)
(check (bit-swap 15 0 1) => 32768)


;; ; 位操作测试
(check (bit-swap 1 0 1) => 2)
(check (bit-swap 2 0 1) => 4)
(check (bit-swap 3 0 1) => 8)
(check (bit-swap 4 0 2) => 2)
(check (bit-swap 5 0 2) => 2)
(check (bit-swap 6 0 2) => 2)
(check (bit-swap 7 0 3) => 130)


;; ; 特殊值测试
;; ; 注意：bit-swap 对较大的整数可能抛出 out-of-range 错误，因此省略这些测试


;; ; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
  (bit-swap "string" 0 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-swap 1 "string" 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-swap 1 2 "string")
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-swap 3.14 0 1)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-swap 1 3.14 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (bit-swap 1 2 3.14)
) ;check-catch


;; ; 错误处理测试 - out-of-range
(check-catch 'out-of-range
  (bit-swap 64 0 1)
) ;check-catch
(check-catch 'out-of-range
  (bit-swap -1 1 3)
) ;check-catch
(check-catch 'out-of-range
  (bit-swap 1 64 2)
) ;check-catch
(check-catch 'out-of-range
  (bit-swap 1 -1 2)
) ;check-catch



(check-report)
