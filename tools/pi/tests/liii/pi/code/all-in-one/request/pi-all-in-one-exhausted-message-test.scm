;; pi-all-in-one-exhausted-message
;; 将全链预检失败转换为 Agent 可结算的 assistant 错误消息。
;;
;; 语法
;; ----
;; (pi-all-in-one-exhausted-message)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; <pi-assistant-message>
;; stopReason=error，错误说明不包含具体供应商凭据。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code all-in-one request)
) ;import

(check-set-mode! 'report-failed)

(let ((message (pi-all-in-one-exhausted-message)))
  (check-true (pi-assistant-message? message))
  (check (pi-assistant-message-stop-reason message) => "error")
  (check (pi-assistant-message-error-message message)
    =>
    "All nodes in the all-in-one chain failed probe"
  ) ;check
) ;let

(check-report)
