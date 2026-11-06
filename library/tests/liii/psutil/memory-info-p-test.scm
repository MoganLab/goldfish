;; memory-info?
;; 判断对象是否为 memory-info 记录。
;;
;; 语法
;; ----
;; (memory-info? x)
;;
;; 参数
;; ----
;; x : any
;; 待检测的对象。
;;
;; 返回值
;; ----
;; boolean
;; x 是 memory-info 记录时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; memory-info 记录由 process-memory-info 创建，
;; 其字段通过 memory-info-resident-set-memory-size、
;; memory-info-virtual-memory-size 与
;; memory-info-proportional-set-memory-size 访问。

(import (liii check)
        (liii psutil))

(check-set-mode! 'report-failed)

(check-true (memory-info? (process-memory-info)))
(check-false (memory-info? '()))
(check-false (memory-info? #f))
(check-false (memory-info? 42))
(check-false (memory-info? '(a b c)))

(check-report)
