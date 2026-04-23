(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; read-string
;; 从输入端口读取指定数量的字符。
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
;; ------
;; string?
;; 读取的字符串，可能短于 k（到达末尾时）。
;;
;; 说明
;; ----
;; 1. 读取 k 个字符或到达 EOF
;; 2. 返回的字符串长度可能小于 k
(let ((p (open-input-string "hello world")))
  (check (read-string 5 p) => "hello")
  (check (read-string 1 p) => " ")
  (check (read-string 5 p) => "world")
) ;let

(check-report)
