;; pi-user-message-role
;; 返回 user 消息的固定角色字符串 "user"。
;;
;; 语法
;; ----
;; (pi-user-message-role msg)
;;
;; 参数
;; ----
;; msg : any?
;; 消息对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "user"。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-user-message-role 'dummy) => "user")

(check-report)
