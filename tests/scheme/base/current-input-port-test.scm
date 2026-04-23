(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; current-input-port
;; 返回当前的输入端口。
;;
;; 语法
;; ----
;; (current-input-port)
;;
;; 返回值
;; ------
;; input-port?
;; 当前默认的输入端口。
;;
;; 说明
;; ----
;; 1. 通常是标准输入
;; 2. 可被 with-input-from-file 临时改变
(check (input-port? (current-input-port)) => #t)

(check-report)
