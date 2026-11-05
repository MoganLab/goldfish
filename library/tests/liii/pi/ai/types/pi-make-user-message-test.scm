;; pi-make-user-message
;; 构造用户消息快捷方式。
;;
;; 语法
;; ----
;; (pi-make-user-message text)
;;
;; 参数
;; ----
;; text : string?
;; 用户输入正文文本。
;;
;; 返回值
;; ----
;; pi-user-message?
;; 用户消息 record 实例。

(import (liii check) (liii pi ai types))

(check-set-mode! 'report-failed)

(let ((msg (pi-make-user-message "hello")))
  (check-true (pi-user-message? msg))
  (check (pi-user-message-role msg) => "user")
) ;let

(check-report)
