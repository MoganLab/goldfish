(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; current-output-port
;; 返回当前的输出端口。
;;
;; 语法
;; ----
;; (current-output-port)
;;
;; 返回值
;; ------
;; output-port?
;; 当前默认的输出端口。
;;
;; 说明
;; ----
;; 1. 通常是标准输出
;; 2. 可被 with-output-to-file 临时改变
(check (output-port? (current-output-port)) => #t)

(check-report)
