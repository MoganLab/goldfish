(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; bytevector-u8-ref
;; 返回字节向量中指定索引位置的字节值。
;;
;; 语法
;; ----
;; (bytevector-u8-ref bv k)
;;
;; 参数
;; ----
;; bv : bytevector?
;; 字节向量。
;;
;; k : integer?
;; 非负的精确整数，表示字节索引位置，必须小于字节向量的长度。
;;
;; 返回值
;; ------
;; integer?
;; 位置k处的字节值，是一个0到255之间的整数。
;;
;; 说明
;; ----
;; 1. 用0基索引访问字节向量中的元素
;; 2. 索引必须是从0到长度减1的非负整数
;; 3. 返回对应位置的字节值
;;
;; 错误处理
;; --------
;; type-error
;; 当bv不是字节向量时或k不是整数时抛出错误。
;; out-of-range
;; 当k小于0或大于等于字节向量长度时抛出错误。

;; bytevector-u8-ref 基本测试
(check (bytevector-u8-ref #u8(5 15 25) 0) => 5)
(check (bytevector-u8-ref #u8(5 15 25) 1) => 15)
(check (bytevector-u8-ref #u8(5 15 25) 2) => 25)
(check (bytevector-u8-ref #u8(255) 0) => 255)
(check (bytevector-u8-ref #u8(0) 0) => 0)


;; 使用其他函数创建的字节向量测试
(check (bytevector-u8-ref (bytevector 10 20 30 40) 0) => 10)
(check (bytevector-u8-ref (bytevector 10 20 30 40) 1) => 20)
(check (bytevector-u8-ref (bytevector 10 20 30 40) 3) => 40)
(check (bytevector-u8-ref (bytevector 200 150 100 50) 2) => 100)

(check (bytevector-u8-ref (make-bytevector 4 99) 0) => 99)
(check (bytevector-u8-ref (make-bytevector 4 99) 3) => 99)
(check (bytevector-u8-ref #u8(1) 0) => 1)  

;; 复杂字节向量测试
(check (bytevector-u8-ref #u8(10 20 30 40 50 60 70 80 90 100) 9) => 100)
(check (bytevector-u8-ref #u8(128 64 32 16 8 4 2 1) 4) => 8)

;; UTF-8转换测试
(check (bytevector-u8-ref (string->utf8 "XYZ") 0) => 88) ;; ASCII 'X'
(check (bytevector-u8-ref (string->utf8 "XYZ") 1) => 89) ;; ASCII 'Y'
(check (bytevector-u8-ref (string->utf8 "A") 0) => 65)

;; 错误处理测试

(check-catch 'wrong-type-arg (bytevector-u8-ref 123 0))
(check-catch 'wrong-type-arg (bytevector-u8-ref "hello" 0))
(check-catch 'wrong-type-arg (bytevector-u8-ref #u8(1 2 3) 1.5))
(check-catch 'out-of-range (bytevector-u8-ref #u8() 0)) ;; empty case
(check-catch 'out-of-range (bytevector-u8-ref #u8(1 2 3) -1))
(check-catch 'out-of-range (bytevector-u8-ref #u8(1 2 3) 3))
(check-catch 'out-of-range (bytevector-u8-ref #u8(1 2 3) 1 3))
(check-catch 'out-of-range (bytevector-u8-ref #u8() 0))
(check-catch 'wrong-number-of-args (bytevector-u8-ref #u8(1 2 3)))

(check-report)
