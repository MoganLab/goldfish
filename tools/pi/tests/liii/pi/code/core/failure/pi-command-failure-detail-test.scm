;; pi-command-failure-detail
;; 提取子进程失败的诊断文本。
;;
;; 语法
;; ----
;; (pi-command-failure-detail err)
;;
;; 参数
;; ----
;; err : process-error
;; 子进程错误信息。
;;
;; 返回值
;; ----
;; string
;; 去除尾部空白后的诊断文本。

(import (liii check)
  (liii string)
  (liii subprocess process-error)
  (liii pi code core failure)
) ;import

(check-set-mode! 'report-failed)

;; 1. 诊断文本为字符串且已去除尾部空白
(let ((detail (pi-command-failure-detail (make-process-error 1 "boom\n" "" "false"))))
  (check-true (string? detail))
  (check-true (> (string-length detail) 0))
  (check-true (string-contains? detail "boom"))
) ;let*

(check-report)
