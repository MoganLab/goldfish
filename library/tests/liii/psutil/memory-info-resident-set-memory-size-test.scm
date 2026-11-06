;; memory-info-resident-set-memory-size
;; 读取 memory-info 记录的驻留物理内存字节数。
;;
;; 语法
;; ----
;; (memory-info-resident-set-memory-size info)
;;
;; 参数
;; ----
;; info : memory-info?
;; 要读取的内存信息记录。
;;
;; 返回值
;; ----
;; exact-integer?
;; 驻留物理内存的字节数，为正精确整数。
;;
;; 错误处理
;; ----
;; type-error
;; info 不是 memory-info 记录时抛出。

(import (liii check)
        (liii psutil))

(check-set-mode! 'report-failed)

(let* ((info (process-memory-info))
       (rss (memory-info-resident-set-memory-size info))
      )
  (check-true (integer? rss))
  (check-true (exact? rss))
  (check-true (> rss 0))
) ;let*

(check-catch 'type-error (memory-info-resident-set-memory-size '()))
(check-catch 'type-error (memory-info-resident-set-memory-size #f))

(check-report)
