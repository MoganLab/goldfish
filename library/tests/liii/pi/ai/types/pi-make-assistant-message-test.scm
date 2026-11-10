;; pi-make-assistant-message
;; 构造助手消息快捷方式。
;;
;; 语法
;; ----
;; (pi-make-assistant-message content)
;; (pi-make-assistant-message content stop-reason)
;; (pi-make-assistant-message content stop-reason error-msg)
;;
;; 参数
;; ----
;; content : any?
;; 消息内容块列表或文本。
;;
;; stop-reason : string?
;; 可选，结束原因（默认为 "stop"）。
;;
;; error-msg : string? 或 #f
;; 可选，错误信息（默认为 #f）。
;;
;; 返回值
;; ----
;; pi-assistant-message?
;; 助手消息 record 实例。

(import (liii check) (liii pi ai types))

(check-set-mode! 'report-failed)

(let ((msg (pi-make-assistant-message "hi" "stop" #f)))
  (check-true (pi-assistant-message? msg))
  (check (pi-assistant-message-role msg) => "assistant")
  (check (pi-assistant-message-stop-reason msg) => "stop")
) ;let

(check-report)
