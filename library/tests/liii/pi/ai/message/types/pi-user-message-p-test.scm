;; pi-user-message?
;; 判断对象是否为用户消息 record 实例。
;;
;; 语法
;; ----
;; (pi-user-message? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于用户消息 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-user-message "你好" 1000)))
  (check-true (pi-user-message? obj))
) ;let
(check-false (pi-user-message? '()))
(check-false (pi-user-message? #f))
(check-false (pi-user-message? "not-a-record"))

(check-report)
