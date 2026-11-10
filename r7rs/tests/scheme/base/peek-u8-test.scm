;; peek-u8
;; 从二进制输入端口预读一个字节，但不消耗该字节。
;;
;; 语法
;; ----
;; (peek-u8)
;; (peek-u8 port)
;;
;; 参数
;; ----
;; port : input-port?
;; 可选，默认为当前输入端口。
;;
;; 返回值
;; ----
;; integer? 或 eof-object?
;; 预读的 0-255 范围内的字节整数，或 EOF 对象。
;;
;; 说明
;; ----
;; 1. 预读后端口位置不前进
;; 2. 连续 peek-u8 返回同一个字节
;; 3. 随后的 read-u8 将返回该字节

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 预读不消耗字节
(let ((p (open-input-bytevector (bytevector 100 200))))
  (check (peek-u8 p) => 100)
  (check (peek-u8 p) => 100)
  (check (read-u8 p) => 100)
  (check (peek-u8 p) => 200)
  (check (read-u8 p) => 200)
  (check (eof-object? (peek-u8 p)) => #t)
  (check (eof-object? (read-u8 p)) => #t)
  (close-port p)
) ;let

;; 默认从 current-input-port 预读
(let ((orig (current-input-port))
      (p (open-input-bytevector (bytevector 42))))
  (set-current-input-port p)
  (check (peek-u8) => 42)
  (check (read-u8) => 42)
  (check (eof-object? (peek-u8)) => #t)
  (set-current-input-port orig)
  (close-port p)
) ;let

;; 错误处理：非输入端口
(check-catch 'type-error (peek-u8 "not-a-port"))
(check-catch 'type-error (peek-u8 123))

;; 错误处理：已关闭的端口
(let ((p (open-input-bytevector (bytevector 1))))
  (close-input-port p)
  (check-catch 'type-error (peek-u8 p))
) ;let

(check-catch 'wrong-number-of-args (peek-u8 1 2))

(check-report)
