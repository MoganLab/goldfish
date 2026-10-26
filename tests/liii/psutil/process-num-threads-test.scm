(import (liii check)
        (liii os)
        (liii psutil))

(check-set-mode! 'report-failed)

;; process-num-threads
;; 查询当前或指定进程当前的线程数。
;;
;; 语法
;; ----
;; (process-num-threads [pid]) => positive-integer?
;;
;; 参数
;; ----
;; pid : 正精确整数，省略时查询当前进程。
;;
;; 错误
;; ----
;; PID 不是精确整数时抛 type-error，非正整数时抛 value-error。
;; 进程不存在时抛 process-not-found-error；线程数无法读取时抛 os-error。

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
