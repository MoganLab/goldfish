(import (liii check) (liii pi ai deepseek) (liii pi ai event) (liii pi ai message) (liii pi ai deepseek))

(check-set-mode! 'report-failed)

;; deepseek-parse-response
;; 解析 DeepSeek/Anthropic 兼容网关返回的完整响应 JSON 对象。
(let* ((resp '(("id" . "msg_123")
               ("type" . "message")
               ("role" . "assistant")
               ("content" . (((type . "text") (text . "DeepSeek Pong"))))
               ("stop_reason" . "end_turn")
               ("usage" . (("input_tokens" . 10) ("output_tokens" . 5)))))
       (parsed (deepseek-parse-response resp)))
  (check (pi-assistant-message? parsed) => #t)
  (check (pi-assistant-message-role parsed) => "assistant")
  (check (pi-assistant-message-stop-reason parsed) => "stop")
  (let ((content (pi-assistant-message-content parsed)))
    (check (pair? content) => #t)
    (check (pi-text-block-text (car content)) => "DeepSeek Pong"))
  (let ((u (pi-assistant-message-usage parsed)))
    (check (cdr (assoc 'input u)) => 10)
    (check (cdr (assoc 'output u)) => 5)))

(check-report)
