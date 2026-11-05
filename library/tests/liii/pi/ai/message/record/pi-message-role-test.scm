;; pi-message-role
;; 获取消息对象的角色（role）。
;;
;; 语法
;; ----
;; (pi-message-role msg)
;;
;; 参数
;; ----
;; msg : any?
;; 消息 record 或 alist。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 角色名称字符串（如 "user"、"assistant" 等）。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-message-role (pi-user-message "hi")) => "user")
(check (pi-message-role (pi-assistant-message "hello")) => "assistant")

(check-report)
