(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; char-ready?
;; 检查输入端口是否有可读字符。
;;
;; 语法
;; ----
;; (char-ready?)
;; (char-ready? port)
;;
;; 参数
;; ----
;; port : input-port?
;; 可选，默认为当前输入端口。
;;
;; 返回值
;; ------
;; boolean?
;; 如果有字符可读则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 用于非阻塞输入检查
;; 2. 到达 EOF 时返回 #t（read-char 将返回 EOF 对象）
(let ((p (open-input-string "a")))
  (check (char-ready? p) => #t)
  (read-char p)
  (check (char-ready? p) => #t)
  (read-char p)
  (check (char-ready? p) => #t)
) ;let

(check-report)
