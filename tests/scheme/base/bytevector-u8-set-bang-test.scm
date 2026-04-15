(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; bytevector-u8-set!
;; 修改字节向量中指定位置的字节值。
;;
;; 语法
;; ----
;; (bytevector-u8-set! bv k byte)
;;
;; 参数
;; ----
;; bv : bytevector?
;; 要修改的字节向量。
;;
;; k : integer?
;; 非负的精确整数，表示字节索引位置，必须小于字节向量的长度。
;;
;; byte : integer?
;; 0到255之间的整数，表示要设置的新的字节值。
;;
;; 返回值
;; ------
;; unspecified
;; 过程修改字节向量后立即返回，没有特定的返回值。
;;
;; 错误处理
;; --------
;; out-of-range
;; 当k小于0或大于等于字节向量长度时抛出错误。
;; wrong-type-arg
;; 当byte不是0-255之间的整数时抛出错误。
(let ((bv (bytevector 1 2 3 4 5)))
  (bytevector-u8-set! bv 1 4)
  (check bv => #u8(1 4 3 4 5))
  (bytevector-u8-set! bv 0 10)
  (check bv => #u8(10 4 3 4 5))
  (bytevector-u8-set! bv 4 255)
  (check bv => #u8(10 4 3 4 255))
) ;let
(let ((bv (bytevector 5)))
  (bytevector-u8-set! bv 0 10)
  (check bv => #u8(10))
) ;let
;; 错误处理测试
(check-catch 'out-of-range
  (bytevector-u8-set! #u() 0 5)
) ;check-catch
(check-catch 'out-of-range
  (bytevector-u8-set! #u8(1 2 3) -1 5)
) ;check-catch
(check-catch 'out-of-range
  (bytevector-u8-set! #u8(1 2 3) 3 5)
) ;check-catch
(check-catch 'wrong-type-arg
  (bytevector-u8-set! 123 0 5)
) ;check-catch
(check-catch 'wrong-type-arg
  (bytevector-u8-set! "hello" 0 5)
) ;check-catch
(check-catch 'wrong-type-arg
  (bytevector-u8-set! #u8(1 2 3) 1 256)
) ;check-catch
(check-catch 'wrong-type-arg
  (bytevector-u8-set! #u8(1 2 3) 1 -1)
) ;check-catch
(check-report)
