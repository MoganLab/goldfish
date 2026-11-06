;; process-error-stdout
;; 获取子进程错误记录保存的标准输出。
;;
;; 语法
;; ----
;; (process-error-stdout err)
;;
;; 参数
;; ----
;; err : process-error
;; 待读取的子进程错误记录。
;;
;; 返回值
;; ----
;; string
;; 子进程的标准输出，未捕获到内容时为空字符串。

(import (liii check) (liii subprocess process-error))

(check-set-mode! 'report-failed)

(check (process-error-stdout (make-process-error 1 "" "partial output\n" "false"))
  =>
  "partial output\n"
) ;check
(check (process-error-stdout (make-process-error 1 "failed\n" "" "false"))
  =>
  ""
) ;check

(check-report)
