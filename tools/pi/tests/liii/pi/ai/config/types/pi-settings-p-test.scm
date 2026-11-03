;; pi-settings?
;; 判断对象是否为客户端全局偏好配置 record 实例。
;;
;; 语法
;; ----
;; (pi-settings? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于客户端全局偏好配置 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(define retry-cfg-a (%make-pi-retry-config #t 5 1500 45000 12))
(let ((obj (%make-pi-settings "google"
             "gemini-2.5-flash"
             "medium"
             "http://127.0.0.1:7890"
             retry-cfg-a
             '("read" "write")
             "/tmp/pi-session"
             #f
             '(("deepseek/deepseek-v4-pro" . "high"))
           ) ;%make-pi-settings
      ) ;obj
     ) ;
  (check-true (pi-settings? obj))
) ;let
(check-false (pi-settings? '()))
(check-false (pi-settings? #f))
(check-false (pi-settings? "not-a-record"))

(check-report)
