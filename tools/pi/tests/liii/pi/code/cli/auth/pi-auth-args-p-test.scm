;; pi-auth-args?
;; 判断对象是否为 <pi-auth-args> 记录。
;;
;; 语法
;; ----
;; (pi-auth-args? obj) -> boolean?
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 若为 <pi-auth-args> 记录则返回 #t，否则返回 #f。

(import (liii check) (liii pi code cli auth))

(check-set-mode! 'report-failed)

(check-true (pi-auth-args? (make-pi-auth-args #t #f #f #f)))
(check-false (pi-auth-args? '((help . #t))))

(check-report)
