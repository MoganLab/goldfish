;; get-output-bytevector
;; 获取二进制输出端口中累积的字节向量。
;;
;; 语法
;; ----
;; (get-output-bytevector port)
;;
;; 参数
;; ----
;; port : output-port?
;; 由 open-output-bytevector 创建的二进制输出端口。
;;
;; 返回值
;; ----
;; bytevector?
;; 包含目前为止写入该端口的所有字节的新分配字节向量。
;;
;; 说明
;; ----
;; 1. 多次调用不会重置或清空端口已累积的内容
;; 2. 端口关闭后不能再调用
;; 3. 传入非 open-output-bytevector 端口抛出 type-error

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 空端口
(let ((p (open-output-bytevector)))
  (check (get-output-bytevector p) => (bytevector))
  (close-port p)
) ;let

;; 累积内容与多次调用非破坏性
(let ((p (open-output-bytevector)))
  (write-u8 10 p)
  (check (get-output-bytevector p) => (bytevector 10))
  (write-u8 20 p)
  (check (get-output-bytevector p) => (bytevector 10 20))
  ;; 再次获取依然一致
  (check (get-output-bytevector p) => (bytevector 10 20))
  (close-port p)
) ;let

;; 包含各类二进制字节
(let ((p (open-output-bytevector)))
  (write-u8 0 p)
  (write-u8 127 p)
  (write-u8 128 p)
  (write-u8 255 p)
  (check (get-output-bytevector p) => (bytevector 0 127 128 255))
  (close-port p)
) ;let

;; 错误处理：关闭的端口
(let ((p (open-output-bytevector)))
  (close-output-port p)
  (check-catch 'type-error (get-output-bytevector p))
) ;let

;; 错误处理：非输出端口或非 bytevector 输出端口
(check-catch 'type-error (get-output-bytevector "not a port"))
(check-catch 'type-error (get-output-bytevector (current-input-port)))
(check-catch 'type-error (get-output-bytevector (current-output-port)))
(check-catch 'wrong-number-of-args (get-output-bytevector))
(check-catch 'wrong-number-of-args (get-output-bytevector 1 2))

(check-report)
