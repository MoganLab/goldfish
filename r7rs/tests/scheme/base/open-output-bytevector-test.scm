;; open-output-bytevector
;; 创建一个用于累积字节的二进制输出端口。
;;
;; 语法
;; ----
;; (open-output-bytevector)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; binary-port?
;; 一个新的二进制输出端口，写入该端口的字节会被累积，
;; 可通过 get-output-bytevector 获取累积的字节向量。
;;
;; 说明
;; ----
;; 1. 创建的端口既是输出端口也是二进制端口
;; 2. 初始时累积的内容为空

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 端口属性
(let ((p (open-output-bytevector)))
  (check-true (output-port? p))
  (check-true (binary-port? p))
  (check (get-output-bytevector p) => (bytevector))
  (close-port p)
) ;let

;; 写入字节后累积
(let ((p (open-output-bytevector)))
  (write-u8 65 p)
  (write-u8 66 p)
  (check (get-output-bytevector p) => (bytevector 65 66))
  (close-port p)
) ;let

;; 参数检查
(check-catch 'wrong-number-of-args (open-output-bytevector 1))

(check-report)
