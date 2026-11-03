;; pi-credential?
;; 判断对象是否为服务商 API 凭据 record 实例。
;;
;; 语法
;; ----
;; (pi-credential? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于服务商 API 凭据 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-credential "google" "secret-key" "api_key")))
  (check-true (pi-credential? obj))
) ;let
(check-false (pi-credential? '()))
(check-false (pi-credential? #f))
(check-false (pi-credential? "not-a-record"))

(check-report)
