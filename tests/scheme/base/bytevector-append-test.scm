(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; bytevector-append
;; 将多个字节向量连接为一个新字节向量。
;;
;; 语法
;; ----
;; (bytevector-append bytevector ...)
;;
;; 参数
;; ----
;; bytevector ... : bytevector?
;; 任意数量的字节向量。
;;
;; 返回值
;; ------
;; bytevector?
;; 所有输入字节向量按顺序连接后的新字节向量。
;;
;; 说明
;; ----
;; 1. 无参数时返回空字节向量
;; 2. 单参数时返回该参数本身
;; 3. 返回新字节向量，不修改原向量
(check (bytevector-append #u8(1)) => #u8(1))
(check (bytevector-append #u8(1 2) #u8(3 4)) => #u8(1 2 3 4))
(check (bytevector-append #u8(1) #u8(2) #u8(3)) => #u8(1 2 3))
(check (bytevector-append #u8() #u8(1)) => #u8(1))
(check (bytevector-append #u8(1) #u8()) => #u8(1))
(check (bytevector-append #u8() #u8()) => #u8())
(let ((v #u8(1 2)))
  (let ((result (bytevector-append v #u8(3))))
    (bytevector-u8-set! result 0 99)
    (check v => #u8(1 2))
    (check result => #u8(99 2 3))
  ) ;let
) ;let
(check-catch 'wrong-type-arg (bytevector-append #u8(1) 'a))

(check-report)
