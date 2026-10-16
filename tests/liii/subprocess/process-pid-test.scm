(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-pid
;; 获取子进程的操作系统进程 ID (PID)。
;; 在进程退出或调用 process-close 之后，该函数仍返回该子进程原有的 PID。
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

(let* ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(0)")))
       (pid (process-pid proc))
      ) ;
  (check (> pid 0) => #t)
  (check (integer? pid) => #t)
  (check (process-wait proc 2000) => 0)
  (check (process-alive? proc) => #f)
  ;; 进程退出后，PID 保持不变
  (check (process-pid proc) => pid)
  ;; process-close 之后，PID 仍保持不变
  (process-close proc)
  (check (process-pid proc) => pid)
) ;let*

;; 类型错误
(check-catch 'type-error (process-pid "invalid"))
(check-catch 'type-error (process-pid 123))
(check-catch 'type-error (process-pid '()))

(check-report)
