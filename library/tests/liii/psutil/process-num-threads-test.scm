;; process-num-threads
;; 查询当前或指定进程当前的线程数。
;;
;; 语法
;; ----
;; (process-num-threads)
;; (process-num-threads pid)
;;
;; 参数
;; ----
;; pid : 正精确整数
;; 要查询的进程号，省略时查询当前进程。
;;
;; 返回值
;; ----
;; exact-integer?
;; 进程的线程数，为正精确整数。
;;
;; 错误处理
;; ----
;; type-error
;; PID 不是精确整数时抛出。
;;
;; value-error
;; PID 不是正整数时抛出。
;;
;; process-not-found-error
;; 进程不存在时抛出。
;;
;; os-error
;; 线程数无法读取时抛出。

(import (liii check)
        (liii os)
        (liii psutil))

(check-set-mode! 'report-failed)

(let ((count (process-num-threads)))
  (check-true (integer? count))
  (check-true (exact? count))
  (check-true (> count 0)))

(let ((count (process-num-threads (getpid))))
  (check-true (exact-integer? count))
  (check-true (> count 0)))

(for-each
  (lambda (pid)
    (check-catch 'type-error (process-num-threads pid)))
  (list "1" 1.0 #t #f 'pid))
(check-catch 'value-error (process-num-threads 0))
(check-catch 'value-error (process-num-threads -1))
(check-catch 'process-not-found-error (process-num-threads 999999))

(check-report)
