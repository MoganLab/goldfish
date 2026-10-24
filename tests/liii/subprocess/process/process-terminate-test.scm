(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-terminate
;; 优雅终止子进程。
;;
;; 语法
;; ----
;; (process-terminate proc)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; 返回值
;; ----
;; 未定义
;;
;; 说明
;; ----
;; POSIX 平台上向子进程发送 SIGTERM 信号；
;; Windows 平台上发送 CTRL_BREAK 或执行 TerminateProcess。

(let ((proc (process-spawn '("python3" "-c" "import time; time.sleep(10)"))))
  (check (process-alive? proc) => #t)
  (process-terminate proc)
  (let ((ret (process-wait proc 2000)))
    (check (integer? ret) => #t)
    (check (process-alive? proc) => #f)
  ) ;let
  (process-close proc)
) ;let

;; 类型错误
(check-catch 'type-error (process-terminate "proc"))
(check-catch 'type-error (process-terminate 123))

(check-report)
