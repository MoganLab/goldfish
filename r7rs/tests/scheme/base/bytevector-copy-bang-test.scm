;; bytevector-copy!
;; 把源字节向量的区间复制到目标字节向量的指定位置。
;;
;; 语法
;; ----
;; (bytevector-copy! to at from)
;; (bytevector-copy! to at from start)
;; (bytevector-copy! to at from start end)
;;
;; 参数
;; ----
;; to : bytevector?
;; 目标字节向量。
;;
;; at : integer?
;; 目标起始下标，0 <= at <= (bytevector-length to)。
;;
;; from : bytevector?
;; 源字节向量。
;;
;; start : integer? (可选)
;; 源起始下标（含），默认 0。
;;
;; end : integer? (可选)
;; 源结束下标（不含），默认 (bytevector-length from)。
;;
;; 返回值
;; ----
;; unspecified
;; 以副作用修改 to。要求 (- (bytevector-length to) at) >= (- end start)。
;;
;; 说明
;; ----
;; R7RS 6.9：源与目标重叠时，按“先把源拷入临时字节向量再拷入目标”
;; 的语义处理，本实现通过 bytevector-copy 临时拷贝天然满足该语义。
;; 注：R6RS 中该过程参数顺序为 (to at from start end) 之外的旧序，
;; R7RS 已固定为本签名。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数类型不正确时抛出错误。
;; out-of-range
;; 当 at/start/end 越界或目标空间不足时抛出错误。

(import (liii check))
(import (liii base))
(import (scheme base))

(check-set-mode! 'report-failed)

;; R7RS 6.9 标准示例
(check
  (let ((a (bytevector 1 2 3 4 5)) (b (bytevector 10 20 30 40 50)))
    (bytevector-copy! b 1 a 0 2)
    b
  ) ;let
  => #u8(10 1 2 40 50)
) ;check

;; 省略 start/end：复制整个源
(check
  (let ((b (bytevector 10 20 30 40 50)))
    (bytevector-copy! b 1 (bytevector 1 2))
    b
  ) ;let
  => #u8(10 1 2 40 50)
) ;check

;; 只省略 end：从 start 复制到源尾
(check
  (let ((b (bytevector 10 20 30 40 50)))
    (bytevector-copy! b 0 (bytevector 1 2 3) 1)
    b
  ) ;let
  => #u8(2 3 30 40 50)
) ;check

;; 完全覆盖目标
(check
  (let ((b (bytevector 0 0 0)))
    (bytevector-copy! b 0 (bytevector 7 8 9))
    b
  ) ;let
  => #u8(7 8 9)
) ;check

;; 空区间：复制零个字节
(check
  (let ((b (bytevector 10 20 30)))
    (bytevector-copy! b 1 (bytevector 1 2) 1 1)
    b
  ) ;let
  => #u8(10 20 30)
) ;check

;; at 等于目标长度，区间为空时不越界
(check
  (let ((b (bytevector 10 20 30)))
    (bytevector-copy! b 3 (bytevector) 0 0)
    b
  ) ;let
  => #u8(10 20 30)
) ;check

;; 源与目标同一字节向量（重叠）
(check
  (let ((b (bytevector 1 2 3 4 5 6)))
    (bytevector-copy! b 0 b 2 5)   ; 取 [2,5) 覆盖头部
    b
  ) ;let
  => #u8(3 4 5 4 5 6)
) ;check
(check
  (let ((b (bytevector 1 2 3 4 5 6)))
    (bytevector-copy! b 3 b 0 3)   ; 取 [0,3) 写到尾部
    b
  ) ;let
  => #u8(1 2 3 1 2 3)
) ;check

;; 越界：目标空间不足
(check-catch 'out-of-range (bytevector-copy! (bytevector 1 2 3) 2 (bytevector 9 9 9)))
;; 越界：at 超过目标长度
(check-catch 'out-of-range (bytevector-copy! (bytevector 1 2 3) 4 (bytevector 9)))
;; 越界：start/end 超出源
(check-catch 'out-of-range (bytevector-copy! (bytevector 1 2 3) 0 (bytevector 9) 0 5))
(check-catch 'out-of-range (bytevector-copy! (bytevector 1 2 3) 0 (bytevector 9) 2))

;; 类型错误
(check-catch 'type-error (bytevector-copy! 'sym 0 (bytevector 1)))
(check-catch 'type-error (bytevector-copy! (bytevector 1) #\a (bytevector 1)))
(check-catch 'type-error (bytevector-copy! (bytevector 1) 0 'sym))
(check-catch 'type-error (bytevector-copy! (bytevector 1) 0 (bytevector 1) #\a))

(check-report)
