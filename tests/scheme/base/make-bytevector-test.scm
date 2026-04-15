(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; make-bytevector
;; 创建一个新的字节向量，指定长度和初始值为所有组成字节。
;;
;; 语法
;; ----
;; (make-bytevector k [fill])
;;
;; 参数
;; ----
;; k : integer?
;; 必须是非负的精确整数，表示字节向量的长度。
;;
;; fill : integer? 可选, 默认为0
;; 0到255之间的整数，作为所有字节的初始值。
;;
;; 返回值
;; ------
;; bytevector?
;; 创建的字节向量，所有元素都设为指定的fill值。
;;
;; 说明
;; ----
;; 1. 可以指定长度和填充值
;; 2. 填充值默认为0，如果提供必须在0-255范围内
;; 3. 特殊字符如#等会被转换为对应的字节值
;;
;; 错误处理
;; --------
;; out-of-range
;; 当k小于0时抛出错误。
;; wrong-type-arg
;; 当任何参数不正确时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1或2个时抛出错误。
;; make-bytevector 基本测试
(check (make-bytevector 0) => #u())
(check (make-bytevector 1) => #u8(0))
(check (make-bytevector 3)
  =>
  #u8(0 0 0)
) ;check
(check (make-bytevector 5 42)
  =>
  #u8(42 42 42 42 42)
) ;check
(check (make-bytevector 2 255)
  =>
  #u8(255 255)
) ;check
;; 不同长度测试
(check (make-bytevector 0 0) => #u())
(check (make-bytevector 1 128)
  =>
  #u8(128)
) ;check
(check (make-bytevector 10 99)
  =>
  #u8(99 99 99 99 99 99 99 99 99 99)
) ;check
;; 边界条件测试
(check (make-bytevector 0) => #u())
(check (make-bytevector 1 0) => #u8(0))
(check (make-bytevector 1 255)
  =>
  #u8(255)
) ;check
;; 特殊值测试
(check (make-bytevector 4 0)
  =>
  #u8(0 0 0 0)
) ;check
(check (make-bytevector 3 170)
  =>
  #u8(170 170 170)
) ;check
(check (make-bytevector 8 255)
  =>
  #u8(255 255 255 255 255 255 255 255)
) ;check
;; 错误处理测试
(check-catch 'out-of-range
  (make-bytevector -5)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-bytevector 3 256)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-bytevector 2 -1)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-bytevector 3.5)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-bytevector "hello")
) ;check-catch
(check-catch 'wrong-number-of-args
  (make-bytevector)
) ;check-catch
(check-catch 'wrong-number-of-args
  (make-bytevector 1 2 3)
) ;check-catch
(check-report)
