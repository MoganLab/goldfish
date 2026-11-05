;; pi-tool-result-message-role
;; 返回 toolResult 消息的固定角色字符串 "toolResult"。
;;
;; 语法
;; ----
;; (pi-tool-result-message-role msg)
;;
;; 参数
;; ----
;; msg : any?
;; 消息对象。
;;
;; 返回值
;; ----
;; string?
;; 字符串 "toolResult"。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check (pi-tool-result-message-role 'dummy) => "toolResult")

(check-report)
