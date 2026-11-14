;; pi-cli-args?
;; 判断对象是否为 <pi-cli-args> 记录。
;;
;; 语法
;; ----
;; (pi-cli-args? obj) -> boolean?
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 若为 <pi-cli-args> 记录则返回 #t，否则返回 #f。

(import (liii check) (liii pi code cli args query))

(check-set-mode! 'report-failed)

(check-true (pi-cli-args? (make-pi-cli-args)))
(check-false (pi-cli-args? '((provider . "google"))))
(check-false (pi-cli-args? #f))

(check-report)
