(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-poll
;; 非阻塞轮询子进程的状态。
;;
;; 语法
;; ----
;; (process-poll proc)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 若进程仍在运行返回 #f；若进程已结束返回其退出码（integer）。

(let ((proc (process-spawn '("python3"
                             "-c"
                             "import time, sys; time.sleep(0.3); sys.exit(7)")
            ) ;process-spawn
      ) ;proc
     ) ;
  (check (process-poll proc) => #f)
  (process-wait proc 2000)
  (check (process-poll proc) => 7)
  (process-close proc)
) ;let

;; 类型错误
(check-catch 'type-error (process-poll #f))
(check-catch 'type-error (process-poll "invalid"))

(check-report)
