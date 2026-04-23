(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; current-error-port
;; 返回当前的错误输出端口。
;;
;; 语法
;; ----
;; (current-error-port)
;;
;; 返回值
;; ------
;; output-port?
;; 当前默认的错误输出端口。
;;
;; 说明
;; ----
;; 1. 通常是标准错误输出
;; 2. 用于输出诊断和错误信息
(check (output-port? (current-error-port)) => #t)

(check-report)
