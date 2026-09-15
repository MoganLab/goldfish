(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; bytevector-accumulator
;; 创建按输入顺序将字节数值收集为 bytevector 的累加器。
;;
;; 语法
;; ----
;; (bytevector-accumulator)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 一个单参累加器过程。传入字节数值时存入内部缓存；传入 eof-object 时按接收顺序返回所收集字节构成的 bytevector。
;;
;; 错误处理
;; ----
;; 若传入非有效字节数值，在 finalize 时可能引发错误。

(let ((a (bytevector-accumulator)))
  (a 1)
  (a 2)
  (check (a (eof-object)) => #u8(1 2))
)

(let ((a (bytevector-accumulator)))
  (check (a (eof-object)) => #u8())
)

(check-report)
