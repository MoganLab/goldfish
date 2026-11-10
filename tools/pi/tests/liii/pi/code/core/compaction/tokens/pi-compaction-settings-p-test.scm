;; pi-compaction-settings?
;; 判断对象是否为自动压缩设置 record。
;;
;; 语法
;; ----
;; (pi-compaction-settings? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; 仅对 pi-compaction-settings record 返回 #t。
;;

(import (scheme base) (liii check) (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)


(check-true (pi-compaction-settings? (make-pi-compaction-settings #t 16384 20000))
) ;check-true
(check-false (pi-compaction-settings? #f))
(check-false (pi-compaction-settings? '()))
(check-false (pi-compaction-settings? (list (cons 'enabled #t))))

(check-report)
