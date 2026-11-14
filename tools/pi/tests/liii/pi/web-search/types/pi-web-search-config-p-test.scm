;; pi-web-search-config?
;; 判断对象是否为 pi-web-search-config 记录。
;;
;; 语法
;; ----
;; (pi-web-search-config? x)
;;
;; 参数
;; ----
;; x : any
;; 待检查对象。
;;
;; 返回值
;; ----
;; boolean
;; 为 pi-web-search-config 记录时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi web-search types))

(check-set-mode! 'report-failed)

(check-true (pi-web-search-config? (make-pi-web-search-config "google" #f #f #f)))
(check-false (pi-web-search-config? '()))
(check-false (pi-web-search-config? #f))

(check-report)
