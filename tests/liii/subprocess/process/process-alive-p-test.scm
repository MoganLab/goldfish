(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-alive?
;; 查询子进程当前是否仍在运行。
;;
;; 语法
;; ----
;; (process-alive? proc)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; 返回值
;; ----
;; boolean
;; 若子进程仍在运行返回 #t，若已结束或已退出返回 #f。

(let ((proc (process-spawn '("python3" "-c" "import time; time.sleep(0.3)"))))
  (check (process-alive? proc) => #t)
  (process-wait proc 2000)
  (check (process-alive? proc) => #f)
  (process-close proc)
) ;let

;; 类型错误
(check-catch 'type-error (process-alive? "invalid"))
(check-catch 'type-error (process-alive? 123))

(check-report)
