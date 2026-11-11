;; pi-boundary-message-stop-reason
;; 统一读取 assistant 消息记录或 alist 的 stopReason。
;;
;; 语法
;; ----
;; (pi-boundary-message-stop-reason message)
;;
;; 参数
;; ----
;; message : any
;; assistant 消息记录、alist 或其他消息。
;;
;; 返回值
;; ----
;; (or string #f)
;; 停止原因；非 assistant 或缺少字段时返回 #f。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code all-in-one boundary)
) ;import

(check-set-mode! 'report-failed)
(check (pi-boundary-message-stop-reason (pi-assistant-message '() "error"))
  =>
  "error"
) ;check
(check (pi-boundary-message-stop-reason '((stopReason . "length"))) => "length")
(check (pi-boundary-message-stop-reason '(("stopReason" . "aborted")))
  =>
  "aborted"
) ;check
(check (pi-boundary-message-stop-reason (pi-user-message "hi")) => #f)

(check-report)
