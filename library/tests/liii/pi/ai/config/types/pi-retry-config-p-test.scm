;; pi-retry-config?
;; 判断对象是否为重试策略配置 record 实例。
;;
;; 语法
;; ----
;; (pi-retry-config? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于重试策略配置 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-retry-config #t 5 1500 45000 12)))
  (check-true (pi-retry-config? obj))
) ;let
(check-false (pi-retry-config? '()))
(check-false (pi-retry-config? #f))
(check-false (pi-retry-config? "not-a-record"))

(check-report)
