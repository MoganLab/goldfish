(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-pid
;; 获取子进程的操作系统进程 ID (PID)。
;;
;; 语法
;; ----
;; (process-pid proc)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; 返回值
;; ----
;; integer
;; 子进程的进程标识符 PID（大于 0 的整数）。

(let ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(0)"))))
  (check (> (process-pid proc) 0) => #t)
  (check (integer? (process-pid proc)) => #t)
  (process-wait proc 2000)
  (process-close proc)
) ;let

;; 类型错误
(check-catch 'type-error (process-pid "invalid"))
(check-catch 'type-error (process-pid 123))
(check-catch 'type-error (process-pid '()))

(check-report)
