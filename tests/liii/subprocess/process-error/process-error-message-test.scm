;; process-error-message
;; 从子进程错误记录中提取便于展示的错误信息。
;;
;; 语法
;; ----
;; (process-error-message err)
;;
;; 参数
;; ----
;; err : process-error
;; 待提取信息的子进程错误记录。
;;
;; 返回值
;; ----
;; string
;; 优先返回非空 stderr，其次返回非空 stdout；两者均为空时返回包含退出码的信息。
;;
;; 错误处理
;; ----
;; err 不是 process-error 记录时抛出 type-error。

(import (liii check) (liii subprocess process-error))

(check-set-mode! 'report-failed)

(check (process-error-message (make-process-error 1 "error\n" "output\n" "false"))
  =>
  "error\n"
) ;check
(check (process-error-message (make-process-error 2 "" "output\n" "false"))
  =>
  "output\n"
) ;check
(check (process-error-message (make-process-error 127 "" "" "missing"))
  =>
  "Command failed with exit code 127"
) ;check

(check-catch 'type-error (process-error-message "invalid"))
(check-catch 'type-error (process-error-message 42))

(check-report)
