;; pi-user-message-timestamp
;; 获取用户消息 record 的 timestamp 字段。
;;
;; 语法
;; ----
;; (pi-user-message-timestamp obj)
;;
;; 参数
;; ----
;; obj : any?
;; 用户消息 record 实例。
;;
;; 返回值
;; ----
;; number?
;; timestamp 字段的值。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-user-message "你好" 1000))
       (obj-b (%make-pi-user-message "再见" 2000))
      ) ;
  (check (pi-user-message-timestamp obj-a) => 1000)
  (check (pi-user-message-timestamp obj-b) => 2000)
) ;let*

(check-report)
