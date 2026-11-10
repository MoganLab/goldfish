;; goldtool-jobs
;; 读取当前的文件级并发 worker 数量，默认为 1（串行执行）。
;;
;; 语法
;; ----
;; (goldtool-jobs)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; number?
;; 当前并发度。

(import (scheme base) (liii check) (liii goldtool parallel))

(check-set-mode! 'report-failed)

(define original-jobs (goldtool-jobs))

;; 默认并发度为 1
(check (goldtool-jobs) => 1)

;; 设置后读取到新值
(set-goldtool-jobs! 3)
(check (goldtool-jobs) => 3)

;; 恢复原值，避免影响其它测试
(set-goldtool-jobs! original-jobs)
(check (goldtool-jobs) => original-jobs)

(check-report)
