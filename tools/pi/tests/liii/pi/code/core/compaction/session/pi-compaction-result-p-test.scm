;; pi-compaction-result?
;; 判断对象是否为 pi-compaction-result 记录。
;;
;; 语法
;; ----
;; (pi-compaction-result? x)
;;
;; 参数
;; ----
;; x : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 为 pi-compaction-result 记录时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi code core compaction session))

(check-set-mode! 'report-failed)

(check-true (pi-compaction-result? (make-pi-compaction-result "c-1" "sum" "e-1" 100 1 2 '())
            ) ;pi-compaction-result?
) ;check-true
(check-false (pi-compaction-result? '()))
(check-false (pi-compaction-result? #f))

(check-report)
