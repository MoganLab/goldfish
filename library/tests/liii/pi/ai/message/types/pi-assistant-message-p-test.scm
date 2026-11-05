;; pi-assistant-message?
;; 判断对象是否为助手消息 record 实例。
;;
;; 语法
;; ----
;; (pi-assistant-message? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于助手消息 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-assistant-message "助手回复正文"
             "stop"
             #f
             '((input . 10) (output . 20))
             3000
           ) ;%make-pi-assistant-message
      ) ;obj
     ) ;
  (check-true (pi-assistant-message? obj))
) ;let
(check-false (pi-assistant-message? '()))
(check-false (pi-assistant-message? #f))
(check-false (pi-assistant-message? "not-a-record"))

(check-report)
