;; read-bytevector
;; 从二进制输入端口读取至多 k 个字节并返回新分配的字节向量。
;;
;; 语法
;; ----
;; (read-bytevector k)
;; (read-bytevector k port)
;;
;; 参数
;; ----
;; k : integer?
;; 要读取的字节数，必须为非负整数。
;;
;; port : input-port? (可选)
;; 二进制输入端口，默认为当前输入端口。
;;
;; 返回值
;; ----
;; bytevector? 或 eof-object?
;; 包含所读取字节的新分配字节向量。若无可用字节且到达末尾则返回 EOF 对象。
;;
;; 说明
;; ----
;; 1. 若 k 为 0，始终返回空字节向量 #u8()
;; 2. 若在读满 k 个字节前遇到文件末尾，返回仅包含已读字节的较短字节向量

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; k = 0 返回空字节向量
(let ((p (open-input-bytevector (bytevector 1 2 3))))
  (check (read-bytevector 0 p) => (bytevector))
  (check (read-u8 p) => 1)
  (close-port p)
) ;let

;; 读满 k 字节
(let ((p (open-input-bytevector (bytevector 10 20 30 40 50))))
  (check (read-bytevector 3 p) => (bytevector 10 20 30))
  (check (read-bytevector 2 p) => (bytevector 40 50))
  (check (eof-object? (read-bytevector 1 p)) => #t)
  (close-port p)
) ;let

;; 可用字节少于 k 个时返回较短的字节向量
(let ((p (open-input-bytevector (bytevector 1 2))))
  (check (read-bytevector 5 p) => (bytevector 1 2))
  (check (eof-object? (read-bytevector 5 p)) => #t)
  (close-port p)
) ;let

;; 默认从 current-input-port 读取
(let ((orig (current-input-port))
      (p (open-input-bytevector (bytevector 65 66 67))))
  (set-current-input-port p)
  (check (read-bytevector 2) => (bytevector 65 66))
  (check (read-bytevector 2) => (bytevector 67))
  (check (eof-object? (read-bytevector 2)) => #t)
  (set-current-input-port orig)
  (close-port p)
) ;let

;; 错误处理：非正整数或负数
(check-catch 'type-error (read-bytevector -1))
(check-catch 'type-error (read-bytevector "3"))

;; 错误处理：已关闭的端口
(let ((p (open-input-bytevector (bytevector 1))))
  (close-input-port p)
  (check-catch 'type-error (read-bytevector 1 p))
) ;let

(check-report)
