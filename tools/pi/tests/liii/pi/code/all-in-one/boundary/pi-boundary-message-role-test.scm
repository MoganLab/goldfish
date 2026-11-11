;; pi-boundary-message-role
;; 统一读取消息记录或 alist 的 role。
;;
;; 语法
;; ----
;; (pi-boundary-message-role message)
;;
;; 参数
;; ----
;; message : any
;; 消息记录、alist 或其他值。
;;
;; 返回值
;; ----
;; (or string #f)
;; 消息角色；无法识别时返回 #f。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code all-in-one boundary)
) ;import

(check-set-mode! 'report-failed)
(check (pi-boundary-message-role (pi-user-message "hi")) => "user")
(check
  (pi-boundary-message-role (pi-assistant-message (list (pi-text-block "ok"))))
  =>
  "assistant"
) ;check
(check (pi-boundary-message-role '((role . "toolResult"))) => "toolResult")
(check (pi-boundary-message-role '(("role" . "user"))) => "user")
(check (pi-boundary-message-role #f) => #f)

(check-report)
