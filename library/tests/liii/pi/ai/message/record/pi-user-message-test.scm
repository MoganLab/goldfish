;; pi-user-message
;; 构造用户消息 record。
;;
;; 语法
;; ----
;; (pi-user-message content)
;; (pi-user-message content timestamp)
;;
;; 参数
;; ----
;; content : any?
;; 用户输入内容（字符串或块列表）。
;;
;; timestamp : integer?
;; 可选，毫秒时间戳。
;;
;; 返回值
;; ----
;; pi-user-message?
;; 用户消息 record 实例。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(let ((msg (pi-user-message "hello" 1234567890)))
  (check-true (pi-user-message? msg))
  (check (pi-user-message-content msg) => "hello")
  (check (pi-user-message-timestamp msg) => 1234567890)
) ;let

(check-report)
