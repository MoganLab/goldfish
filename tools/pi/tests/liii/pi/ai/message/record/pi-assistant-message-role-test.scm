;; pi-assistant-message-role
;; 返回 assistant 消息的固定角色字符串 "assistant"。
;;
;; 语法
;; ----
;; (pi-assistant-message-role msg)
;;
;; 参数
;; ----
;; msg : any?
;; 消息对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "assistant"。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-assistant-message-role 'dummy) => "assistant")

(check-report)
