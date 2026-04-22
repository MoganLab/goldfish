(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; close-input-port
;; 关闭输入端口。
;;
;; 语法
;; ----
;; (close-input-port port)
;;
;; 参数
;; ----
;; port : input-port?
;; 要关闭的输入端口。
;;
;; 返回值
;; ------
;; 未指定
;;
;; 说明
;; ----
;; 1. 关闭后不能再从该端口读取
;; 2. 多次关闭是安全的
(let ((p (open-input-string "test")))
  (close-input-port p)
  (check (input-port? p) => #t)
) ;let

(check-report)
