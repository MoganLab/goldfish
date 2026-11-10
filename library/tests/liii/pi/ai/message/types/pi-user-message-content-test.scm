;; pi-user-message-content
;; 获取用户消息 record 的 content 字段。
;;
;; 语法
;; ----
;; (pi-user-message-content obj)
;;
;; 参数
;; ----
;; obj : any?
;; 用户消息 record 实例。
;;
;; 返回值
;; ----
;; any?
;; content 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-user-message "你好" 1000))
       (obj-b (%make-pi-user-message "再见" 2000))
      ) ;
  (check (pi-user-message-content obj-a) => "你好")
  (check (pi-user-message-content obj-b) => "再见")
) ;let*

(check-report)
