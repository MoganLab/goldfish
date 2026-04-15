(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; bytevector-copy
;; 创建一个新的字节向量，它是现有字节向量的完整或部分副本。
;;
;; 语法
;; ----
;; (bytevector-copy bv [start [end]])
;;
;; 参数
;; ----
;; bv : bytevector?
;; 要被复制的源字节向量。
;;
;; start : integer? 可选, 默认为0
;; 非负的精确整数，表示复制开始的索引位置，必须小于bv的长度。
;;
;; end : integer? 可选, 默认为(bytevector-length bv)
;; 非负的精确整数，表示复制结束的索引位置（不包括该位置），必须大于等于start且小于等于bv的长度。
;;
;; 返回值
;; ------
;; bytevector?
;; 新的字节向量，包含从start到end-1位置的元素副本。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当bv不是字节向量抛出错误。
;;
;; out-of-range
;; 当start或end超出有效范围时抛出错误。
;;
;; wrong-number-of-args
;; 参数数量不正确时抛出错误。
;; bytevector-copy 基本测试
(check (bytevector-copy #u()) => #u())
(check (bytevector-copy #u(1 2 3))
  =>
  #u(1 2 3)
) ;check
(check (bytevector-copy #u(255 0 128))
  =>
  #u(255 0 128)
) ;check
;; 段复制测试
(check (bytevector-copy #u(1 2 3 4 5) 0 3)
  =>
  #u(1 2 3)
) ;check
(check (bytevector-copy #u(1 2 3 4 5) 1 4)
  =>
  #u(2 3 4)
) ;check
(check (bytevector-copy #u(1 2 3 4 5) 2)
  =>
  #u(3 4 5)
) ;check
;; 边界测试
(check (bytevector-copy #u(50 100 150) 0 0)
  =>
  #u()
) ;check
(check (bytevector-copy #u(50 100 150) 0 1)
  =>
  #u(50)
) ;check
(check (bytevector-copy #u(50 100 150) 2 3)
  =>
  #u(150)
) ;check
;; 完整范围
(check (bytevector-copy #u(10 20 30 40 50) 0 5)
  =>
  #u(10 20 30 40 50)
) ;check
;; 独立对象测试
(let ((bv (bytevector 1 2 3 4 5)))
  (check (bytevector-copy bv 1 4)
    =>
    #u(2 3 4)
  ) ;check
) ;let
;; 错误处理
(check-catch 'wrong-type-arg
  (bytevector-copy 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (bytevector-copy "hello")
) ;check-catch
(check-catch 'out-of-range
  (bytevector-copy #u(1 2 3) -1)
) ;check-catch
(check-catch 'out-of-range
  (bytevector-copy #u(1 2 3) 4)
) ;check-catch
(check-catch 'out-of-range
  (bytevector-copy #u(1 2 3) 0 5)
) ;check-catch
(check-catch 'out-of-range
  (bytevector-copy #u(1 2 3) 2 1)
) ;check-catch
(check (bytevector-append #u() #u())
  =>
  #u()
) ;check
(check (bytevector-append #u() #u(1))
  =>
  #u(1)
) ;check
(check (bytevector-append #u(1) #u())
  =>
  #u(1)
) ;check
(check-report)