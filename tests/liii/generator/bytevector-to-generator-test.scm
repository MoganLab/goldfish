(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; bytevector->generator
;; 将字节向量转换为依次产出其中字节数值的生成器。
;;
;; 语法
;; ----
;; (bytevector->generator bv)
;; (bytevector->generator bv start)
;; (bytevector->generator bv start end)
;;
;; 参数
;; ----
;; bv : bytevector
;; 待转换的字节向量。
;;
;; start : exact-nonnegative-integer (可选)
;; 起始索引（包含），默认为 0。
;;
;; end : exact-nonnegative-integer (可选)
;; 结束索引（不包含），默认为 (bytevector-length bv)。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。依次产出字节整数，耗尽时返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (bytevector->generator #u8(1 2))))
  (check (g) => 1)
  (check (g) => 2)
  (check-true (eof-object? (g)))
)

(let ((g (bytevector->generator #u8(10 20 30 40) 1 3)))
  (check (g) => 20)
  (check (g) => 30)
  (check-true (eof-object? (g)))
)

(let ((g (bytevector->generator #u8(10 20 30) 1)))
  (check (g) => 20)
  (check (g) => 30)
  (check-true (eof-object? (g)))
)

(check-report)
