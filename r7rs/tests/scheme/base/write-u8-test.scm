;; write-u8
;; 向二进制输出端口写入一个字节。
;;
;; 语法
;; ----
;; (write-u8 byte)
;; (write-u8 byte port)
;;
;; 参数
;; ----
;; byte : integer?
;; 0-255 范围内的无符号字节整数。
;;
;; port : output-port? (可选)
;; 输出端口。省略时写入当前输出端口。
;;
;; 返回值
;; ----
;; unspecified
;; 主要用于副作用输出。
;;
;; 错误处理
;; --------
;; type-error 当字节不是 0-255 整数或端口不是有效输出端口时抛出。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 写入指定端口
(let ((p (open-output-bytevector)))
  (write-u8 65 p)
  (write-u8 255 p)
  (write-u8 0 p)
  (check (get-output-bytevector p) => (bytevector 65 255 0))
  (close-port p)
) ;let

;; 默认写入 current-output-port
(let ((orig (current-output-port))
      (p (open-output-bytevector)))
  (set-current-output-port p)
  (write-u8 77)
  (write-u8 88)
  (set-current-output-port orig)
  (check (get-output-bytevector p) => (bytevector 77 88))
  (close-port p)
) ;let

;; 错误处理：超出 0-255 范围
(let ((p (open-output-bytevector)))
  (check-catch 'type-error (write-u8 256 p))
  (check-catch 'type-error (write-u8 -1 p))
  (check-catch 'type-error (write-u8 "A" p))
  (close-port p)
) ;let

;; 错误处理：已关闭的端口
(let ((p (open-output-bytevector)))
  (close-output-port p)
  (check-catch 'type-error (write-u8 65 p))
) ;let

(check-report)
