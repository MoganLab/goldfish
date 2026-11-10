;; open-input-bytevector
;; 将一个字节向量转换为二进制输入端口。
;;
;; 语法
;; ----
;; (open-input-bytevector bytevector)
;;
;; 参数
;; ----
;; bytevector : bytevector?
;; 一个字节向量。
;;
;; 返回值
;; ----
;; binary-port?
;; 一个二进制输入端口，该端口会从给定的字节向量中按顺序提供字节。
;;
;; 说明
;; ----
;; 1. 传入非 bytevector 对象会抛出 type-error
;; 2. 读取到末尾时返回 EOF 对象

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 空字节向量
(let ((p (open-input-bytevector (bytevector))))
  (check-true (input-port? p))
  (check-true (binary-port? p))
  (check (eof-object? (read-u8 p)) => #t)
  (close-port p)
) ;let

;; 顺序读取字节
(let ((p (open-input-bytevector (bytevector 1 2 3))))
  (check-true (input-port? p))
  (check-true (binary-port? p))
  (check (read-u8 p) => 1)
  (check (read-u8 p) => 2)
  (check (read-u8 p) => 3)
  (check (eof-object? (read-u8 p)) => #t)
  (close-port p)
) ;let

;; 包含 0 和 255 的二进制字节
(let ((p (open-input-bytevector (bytevector 0 128 255))))
  (check (read-u8 p) => 0)
  (check (read-u8 p) => 128)
  (check (read-u8 p) => 255)
  (check (eof-object? (read-u8 p)) => #t)
  (close-port p)
) ;let

;; 参数类型检查
(check-catch 'type-error (open-input-bytevector "not a bytevector"))
(check-catch 'type-error (open-input-bytevector 123))
(check-catch 'wrong-number-of-args (open-input-bytevector))
(check-catch 'wrong-number-of-args (open-input-bytevector (bytevector) 1))

(check-report)
