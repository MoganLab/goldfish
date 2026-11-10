;; process-memory-info
;; 查询当前或指定进程的内存信息，返回 memory-info record。
;;
;; 语法
;; ----
;; (process-memory-info)
;; (process-memory-info pid)
;;
;; 参数
;; ----
;; pid : 正精确整数
;; 要查询的进程号，省略时查询当前进程。
;;
;; 返回值
;; ----
;; memory-info?
;; 包含驻留物理内存、虚拟内存与按比例分摊内存的记录对象，
;; 各字段通过 memory-info-* 访问器读取。
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

(import (liii check)
        (liii os)
        (liii psutil))

(check-set-mode! 'report-failed)

(define info (process-memory-info))
(check-true (memory-info? info))

(let ((explicit-info (process-memory-info (getpid))))
  (check-true (memory-info? explicit-info))
  (check-true (> (memory-info-resident-set-memory-size explicit-info) 0))
  (check-true (> (memory-info-virtual-memory-size explicit-info) 0)))

(for-each
  (lambda (pid)
    (check-catch 'type-error (process-memory-info pid)))
  (list "1" 1.0 #t #f 'pid))
(check-catch 'value-error (process-memory-info 0))
(check-catch 'value-error (process-memory-info -1))
(check-catch 'process-not-found-error (process-memory-info 999999))

(check-report)
