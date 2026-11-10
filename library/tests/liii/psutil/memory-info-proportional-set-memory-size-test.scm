;; memory-info-proportional-set-memory-size
;; 读取 memory-info 记录按比例分摊的物理内存字节数。
;;
;; 语法
;; ----
;; (memory-info-proportional-set-memory-size info)
;;
;; 参数
;; ----
;; info : memory-info?
;; 要读取的内存信息记录。
;;
;; 返回值
;; ----
;; exact-integer? 或 #f
;; 按比例分摊的物理内存字节数；
;; 平台不支持或无法取得该数值时返回 #f。
;;
;; 说明
;; ----
;; Linux 平台可直接提供该数值；
;; 其他平台可能返回 #f，调用方需自行处理。
;;
;; 错误处理
;; ----
;; type-error
;; info 不是 memory-info 记录时抛出。

(import (liii check)
        (liii os)
        (liii psutil))

(check-set-mode! 'report-failed)

(let ((pss (memory-info-proportional-set-memory-size (process-memory-info))))
  (if (os-linux?)
      (begin
        (check-true (integer? pss))
        (check-true (exact? pss))
        (check-true (>= pss 0)))
      (check-true (or (not pss)
                      (and (exact-integer? pss) (>= pss 0))))))

(check-catch 'type-error (memory-info-proportional-set-memory-size '()))
(check-catch 'type-error (memory-info-proportional-set-memory-size #f))

(check-report)
