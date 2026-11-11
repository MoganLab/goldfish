;; read-u8
;; 从二进制输入端口读取一个字节。
;;
;; 语法
;; ----
;; (read-u8)
;; (read-u8 port)
;;
;; 参数
;; ----
;; port : input-port?
;; 可选，默认为当前输入端口。
;;
;; 返回值
;; ----
;; integer? 或 eof-object?
;; 读取的 0-255 范围内的字节整数，或 EOF 对象。
;;
;; 说明
;; ----
;; 1. 读取后端口位置前进一个字节
;; 2. 到达末尾返回 EOF 对象

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 从指定端口读取
(let ((p (open-input-bytevector (bytevector 65 66 255 0))))
  (check (read-u8 p) => 65)
  (check (read-u8 p) => 66)
  (check (read-u8 p) => 255)
  (check (read-u8 p) => 0)
  (check (eof-object? (read-u8 p)) => #t)
  (close-port p)
) ;let

;; 默认从 current-input-port 读取
(let ((orig (current-input-port))
      (p (open-input-bytevector (bytevector 10 20))))
  (set-current-input-port p)
  (check (read-u8) => 10)
  (check (read-u8) => 20)
  (check (eof-object? (read-u8)) => #t)
  (set-current-input-port orig)
  (close-port p)
) ;let

;; 错误处理：非输入端口
(check-catch 'type-error (read-u8 "not-a-port"))
(check-catch 'type-error (read-u8 123))

;; 错误处理：已关闭的端口
(let ((p (open-input-bytevector (bytevector 1))))
  (close-input-port p)
  (check-catch 'type-error (read-u8 p))
) ;let

;; 错误处理：文本端口不是二进制端口
(let ((p (open-input-string "abc")))
  (check-catch 'type-error (read-u8 p))
) ;let

(check-catch 'wrong-number-of-args (read-u8 1 2))

(check-report)
