(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; anthropic-parse-response
;; 解析 Anthropic API 返回的 JSON 响应。
(let* ((json-resp '(("id" . "msg_123")
                    ("type" . "message")
                    ("role" . "assistant")
                    ("content" . #((("type" . "text") ("text" . "Done!"))))
                    ("model" . "claude-3-7-sonnet-20250219")
                    ("stop_reason" . "end_turn")
                    ("usage" . (("input_tokens" . 10) ("output_tokens" . 5)))))
       (parsed (anthropic-parse-response json-resp 200)))
  (check (cdr (assoc 'role parsed)) => "assistant")
  (check (cdr (assoc 'stopReason parsed)) => "stop")
  (let ((content (cdr (assoc 'content parsed))))
    (check (cdr (assoc 'text (car content))) => "Done!")))

(check-report)
