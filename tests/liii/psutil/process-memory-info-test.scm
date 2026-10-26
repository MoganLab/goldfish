(import (liii check)
        (liii os)
        (liii psutil))

(check-set-mode! 'report-failed)

;; process-memory-info
;; 查询当前或指定进程的内存信息，返回 memory-info record。
;;
;; 语法
;; ----
;; (process-memory-info [pid]) => memory-info?
;;
;; 参数
;; ----
;; pid : 正精确整数，省略时查询当前进程。
;;
;; 访问器
;; ------
;; memory-info-resident-set-memory-size 返回驻留物理内存字节数。
;; memory-info-virtual-memory-size 返回虚拟内存字节数。
;; memory-info-proportional-set-memory-size 返回按比例分摊的物理内存
;; 字节数；平台不支持或无法取得时返回 #f。
;;
;; 错误
;; ----
;; PID 不是精确整数时抛 type-error，非正整数时抛 value-error。
;; 进程不存在时抛 process-not-found-error。

(define info (process-memory-info))
(check-true (memory-info? info))
(check-false (memory-info? '()))
(check-false (memory-info? #f))
(check-true (integer? (memory-info-resident-set-memory-size info)))
(check-true (exact? (memory-info-resident-set-memory-size info)))
(check-true (> (memory-info-resident-set-memory-size info) 0))
(check-true (integer? (memory-info-virtual-memory-size info)))
(check-true (exact? (memory-info-virtual-memory-size info)))
(check-true (> (memory-info-virtual-memory-size info) 0))

(let ((pss (memory-info-proportional-set-memory-size info)))
  (if (os-linux?)
      (begin
        (check-true (integer? pss))
        (check-true (exact? pss))
        (check-true (>= pss 0)))
      (check-true (or (not pss)
                      (and (exact-integer? pss) (>= pss 0))))))

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
