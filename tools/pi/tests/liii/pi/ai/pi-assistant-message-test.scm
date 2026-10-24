(import (liii check) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; pi-assistant-message
;; 构造 assistant 角色消息 record 对象。
(let* ((content (list (pi-text-block "thinking...")))
       (msg (pi-assistant-message content :stop-reason "stop" :error-message #f :usage '((input . 10) (output . 20) (total . 30)))))
  ;; record 谓词与访问器
  (check-true (pi-assistant-message? msg))
  (check-true (pi-message? msg))
  (check (pi-message-role msg) => "assistant")
  (check (pi-assistant-message-content msg) => content)
  (check (pi-assistant-message-stop-reason msg) => "stop")
  (check (pi-assistant-message-error-message msg) => #f)
  (check (pi-assistant-message-usage msg) => '((input . 10) (output . 20) (total . 30)))
  (check-true (number? (pi-assistant-message-timestamp msg)))

  ;; 默认参数构造
  (let ((default-msg (pi-assistant-message "simple text")))
    (check (pi-assistant-message-content default-msg) => "simple text")
    (check (pi-assistant-message-stop-reason default-msg) => "stop")
    (check (pi-assistant-message-error-message default-msg) => #f))

  ;; 兼容 alist 访问
  (check (cdr (assoc "role" msg)) => "assistant")
  (check (cdr (assoc 'role msg)) => "assistant")
  (check (cdr (assoc "content" msg)) => content)
  (check (cdr (assoc 'content msg)) => content)
  (check (cdr (assoc "stopReason" msg)) => "stop")
  (check (cdr (assoc 'stopReason msg)) => "stop")
  (check (cdr (assoc "errorMessage" msg)) => #f)
  (check (cdr (assoc 'errorMessage msg)) => #f))

(check-report)
