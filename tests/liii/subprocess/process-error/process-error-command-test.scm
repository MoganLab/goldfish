;; process-error-command
;; 获取子进程错误记录保存的原始命令。
;;
;; 语法
;; ----
;; (process-error-command err)
;;
;; 参数
;; ----
;; err : process-error
;; 待读取的子进程错误记录。
;;
;; 返回值
;; ----
;; string 或 list
;; 调用执行接口时使用的原始命令，保持其原有形式。

(import (liii check) (liii subprocess process-error))

(check-set-mode! 'report-failed)

(check (process-error-command (make-process-error 1 "" "" "false")) => "false")
(check (process-error-command (make-process-error 1 "" "" '(false)))
  =>
  '(false)
) ;check

(check-report)
