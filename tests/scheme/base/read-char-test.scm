(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; read-char
;; 从输入端口读取一个字符。
;;
;; 语法
;; ----
;; (read-char)
;; (read-char port)
;;
;; 参数
;; ----
;; port : input-port?
;; 可选，默认为当前输入端口。
;;
;; 返回值
;; ------
;; char? 或 eof-object?
;; 读取的字符，或 EOF 对象。
;;
;; 说明
;; ----
;; 1. 读取后端口位置前进一个字符
;; 2. 到达末尾返回 EOF 对象
(let ((p (open-input-string "abc")))
  (check (read-char p) => #\a)
  (check (read-char p) => #\b)
  (check (read-char p) => #\c)
  (check (eof-object? (read-char p)) => #t)
) ;let

(check-report)
