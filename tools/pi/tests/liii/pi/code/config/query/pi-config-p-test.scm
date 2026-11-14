;; pi-config?
;; 判断对象是否为 <pi-config> 记录。
;;
;; 语法
;; ----
;; (pi-config? obj) -> boolean?
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 若为 <pi-config> 记录则返回 #t，否则返回 #f。

(import (liii check) (liii pi code config query))

(check-set-mode! 'report-failed)

(check-true (pi-config? (make-pi-config)))
(check-false (pi-config? '((provider . "google"))))
(check-false (pi-config? #f))

(check-report)
