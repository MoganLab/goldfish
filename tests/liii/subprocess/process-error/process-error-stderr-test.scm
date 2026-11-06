;; process-error-stderr
;; 获取子进程错误记录保存的标准错误输出。
;;
;; 语法
;; ----
;; (process-error-stderr err)
;;
;; 参数
;; ----
;; err : process-error
;; 待读取的子进程错误记录。
;;
;; 返回值
;; ----
;; string
;; 子进程的标准错误输出，未捕获到内容时为空字符串。

(import (liii check) (liii subprocess process-error))

(check-set-mode! 'report-failed)

(check (process-error-stderr (make-process-error 1 "failed\n" "" "false"))
  =>
  "failed\n"
) ;check
(check (process-error-stderr (make-process-error 1 "" "output\n" "false"))
  =>
  ""
) ;check

(check-report)
