(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; bytevector-length
;; 返回字节向量中的元素个数。
;;
;; 语法
;; ----
;; (bytevector-length bv)
;;
;; 参数
;; ----
;; bv : bytevector?
;; 字节向量。
;;
;; 返回值
;; ------
;; integer?
;; 字节向量中的元素数量。
;;
;; 说明
;; ----
;; 1. 返回字节向量中的字节数
;; 2. 空字节向量返回0
;; 3. 结果是非负的精确整数
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是字节向量时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。

;; bytevector-length 基本测试
(check (bytevector-length #u8()) => 0)
(check (bytevector-length #u8(1)) => 1)
(check (bytevector-length #u8(1 2 3)) => 3)
(check (bytevector-length #u8(255)) => 1)
(check (bytevector-length #u8(1 2 3 4 5 6 7 8 9 10)) => 10)

;; 使用不同类型创建的字节向量测试
(check (bytevector-length (bytevector)) => 0)
(check (bytevector-length (bytevector 50 150 250)) => 3)
(check (bytevector-length (make-bytevector 5 42)) => 5)
(check (bytevector-length (make-bytevector 10 0)) => 10)
(check (bytevector-length (make-bytevector 0)) => 0)
(check (bytevector-length "hello") => 5)
(check (bytevector-length 123) => #f)
(check (bytevector-length 'symbol) => #f)

;; 错误处理测试

(check-catch 'wrong-number-of-args (bytevector-length))
(check-catch 'wrong-number-of-args (bytevector-length #u8(1 2 3) #u8(4 5)))

(check-report)
