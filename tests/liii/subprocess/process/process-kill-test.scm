(import (liii check) (liii subprocess))

(check-set-mode! 'report-failed)

;; process-kill
;; 强制杀死子进程。
;;
;; 语法
;; ----
;; (process-kill proc)
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
;; POSIX 平台上向子进程发送 SIGKILL 信号；
;; Windows 平台上调用 TerminateProcess 强行终止子进程。

(let ((proc (process-spawn '("python3" "-c" "import time; time.sleep(10)"))))
  (check (process-alive? proc) => #t)
  (process-kill proc)
  (let ((ret (process-wait proc 2000)))
    (check (integer? ret) => #t)
    (check (process-alive? proc) => #f)
  ) ;let
  (process-close proc)
) ;let

;; 类型错误
(check-catch 'type-error (process-kill 0))
(check-catch 'type-error (process-kill #f))

(check-report)
