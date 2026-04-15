(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; bytevector
;; 返回一个新分配的字节向量，其元素包含传递给过程的所有参数。每个参数都必须是一个介于0到255之间的整数，表示字节向量中的一个字节。如果没有提供任何参数，将创建一个空的字节向量。
;;
;; 语法
;; ----
;; (bytevector byte ...)
;;
;; 参数
;; ----
;; byte... : integer?
;; 零个或多个介于0到255之间的整数（包含边界），表示字节值。
;;
;; 返回值
;; ------
;; bytevector?
;; 新创建的字节向量，包含所有参数指定的字节值。
;;
;; 说明
;; ----
;; 1. 可以接受零个或多个参数
;; 2. 每个参数必须在0-255的范围内
;; 3. 无参数时创建空字节向量
;; 4. 参数顺序就是字节向量中元素的顺序
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当任何参数不是在0-255范围内的整数时抛出错误。

;; bytevector 基本测试
(check (bytevector) => #u8())
(check (bytevector 255) => #u8(255))
(check (bytevector 1 2 3 4) => #u8(1 2 3 4))
(check (bytevector 10 20 30 40 50) => #u8(10 20 30 40 50))

;; 边界测试
(check (bytevector 0) => #u8(0))
(check (bytevector 255) => #u8(255))
(check (bytevector 0 255) => #u8(0 255))

;; 不同长度测试
(check (bytevector) => #u8())
(check (bytevector 15) => #u8(15))
(check (bytevector 85 170) => #u8(85 170))
(check (bytevector 1 2 3 4 5 6 7 8 9 10) => #u8(1 2 3 4 5 6 7 8 9 10))

;; 错误处理测试
(check-catch 'wrong-type-arg (bytevector 256))
(check-catch 'wrong-type-arg (bytevector -1))
(check-catch 'wrong-type-arg (bytevector 123.0))
(check-catch 'wrong-type-arg (bytevector 123 #u8(1 2 3)))

(check-report)
