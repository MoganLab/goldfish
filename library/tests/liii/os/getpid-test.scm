;; getpid
;; 获取当前进程的进程 ID。
;;
;; 语法
;; ----
;; (getpid)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; exact-integer?
;; 当前进程的 PID，为正整数。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 返回正整数 PID
(let ((pid (getpid)))
  (check-true (exact-integer? pid))
  (check-true (> pid 0))
) ;let

;; 同一进程内多次调用结果一致
(check (getpid) => (getpid))

(check-report)
