;; read-string
;; 从输入端口读取指定数量的字符。
;;
;;
;; 语法
;; ----
;; (read-string k)
;; (read-string k port)
;;
;; 参数
;; ----
;; k : exact-integer?
;; 要读取的字符数。
;; port : input-port?
;; 可选，默认为当前输入端口。
;;
;; 返回值
;; ----
;; string?
;; 读取的字符串，可能短于 k（到达末尾时）。
;;
;; 说明
;; ----
;; 1. 读取 k 个字符或到达 EOF
;; 2. 返回的字符串长度可能小于 k

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(let ((p (open-input-string "hello world")))
  (check (read-string 5 p) => "hello")
  (check (read-string 1 p) => " ")
  (check (read-string 5 p) => "world")
) ;let

;; 错误处理：二进制端口不是文本端口
(let ((p (open-input-bytevector (bytevector 1 2 3))))
  (check-catch 'type-error (read-string 2 p))
) ;let

;; 错误处理：已关闭的端口
(let ((p (open-input-string "hello")))
  (close-input-port p)
  (check-catch 'type-error (read-string 2 p))
) ;let

;; 错误处理：非法的 k
(let ((p (open-input-string "hello")))
  (check-catch 'type-error (read-string -1 p))
  (check-catch 'type-error (read-string "abc" p))
) ;let

(check-report)
