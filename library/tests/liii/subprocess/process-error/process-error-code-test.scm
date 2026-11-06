;; process-error-code
;; 获取子进程错误记录中的退出码。
;;
;; 语法
;; ----
;; (process-error-code err)
;;
;; 参数
;; ----
;; err : process-error
;; 由 make-process-error 构造或 run-either 返回的错误记录。
;;
;; 返回值
;; ----
;; integer
;; 失败子进程的退出码。

(import (liii check) (liii subprocess process-error))

(check-set-mode! 'report-failed)

(check (process-error-code (make-process-error 1 "" "" "false")) => 1)
(check (process-error-code (make-process-error 127 "" "" "missing")) => 127)

(check-report)
