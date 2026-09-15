(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; bytevector-accumulator!
;; 创建原地填充指定 bytevector 的累加器。
;;
;; 语法
;; ----
;; (bytevector-accumulator! bytevec at)
;;
;; 参数
;; ----
;; bytevec : bytevector
;; 目标填充字节向量。
;;
;; at : exact-nonnegative-integer
;; 写入 bytevec 的起始索引。
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程。传入字节数值时依次在 bytevec 相应索引位置赋值；传入 eof-object 时返回该 bytevec。
;;
;; 错误处理
;; ----
;; 若写入字节数量超出 bytevec 剩余容量，可能引发越界错误。

(let* ((bv (make-bytevector 2))
       (a (bytevector-accumulator! bv 0)))
  (a 1)
  (a 2)
  (check (a (eof-object)) => #u8(1 2))
  (check bv => #u8(1 2))
)

(let* ((bv (make-bytevector 3))
       (a (bytevector-accumulator! bv 1)))
  (a 1)
  (a 2)
  (check (a (eof-object)) => #u8(0 1 2))
  (check bv => #u8(0 1 2))
)

(check-report)
