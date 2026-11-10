;; pi-message?
;; 判断对象是否为合法的消息 record。
;;
;; 语法
;; ----
;; (pi-message? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待检验的对象。
;;
;; 返回值
;; ----
;; boolean
;; 当为 user、assistant 或 toolResult 消息 record 时返回 #t。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check-true (pi-message? (pi-user-message "hi")))
(check-true (pi-message? (pi-assistant-message "hello")))
(check-false (pi-message? "not a message"))

(check-report)
