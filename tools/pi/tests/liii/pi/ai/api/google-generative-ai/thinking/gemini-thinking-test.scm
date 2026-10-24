(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

;; 1. gemini-thinking-level->google-level 级别映射
(check (gemini-thinking-level->google-level "minimal") => "MINIMAL")
(check (gemini-thinking-level->google-level "low") => "LOW")
(check (gemini-thinking-level->google-level "medium") => "MEDIUM")
(check (gemini-thinking-level->google-level "high") => "HIGH")
;; xhigh/max 钳制到 high
(check (gemini-thinking-level->google-level "xhigh") => "HIGH")
(check (gemini-thinking-level->google-level "max") => "HIGH")
(check (gemini-thinking-level->google-level "off") => #f)
(check (gemini-thinking-level->google-level "unknown") => #f)

;; 2. gemini-build-thinking-config
(check (gemini-build-thinking-config "off") => #f)
(check (gemini-build-thinking-config #f) => #f)

(let ((cfg (gemini-build-thinking-config "low")))
  (check-true (pair? cfg))
  (let ((tc (cdr (assoc "thinkingConfig" cfg))))
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "LOW")))

(let ((cfg (gemini-build-thinking-config "high")))
  (check-true (pair? cfg))
  (let ((tc (cdr (assoc "thinkingConfig" cfg))))
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "HIGH")))

;; 2.1 gemini-inject-thinking-config
(let* ((base '(("contents" . #())))
       (injected (gemini-inject-thinking-config base "low")))
  (check-true (pair? (assoc "generationConfig" injected)))
  (let* ((gc (cdr (assoc "generationConfig" injected)))
         (tc (cdr (assoc "thinkingConfig" gc))))
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "LOW")))

(let* ((base '(("contents" . #())))
       (injected (gemini-inject-thinking-config base "off")))
  (check-false (assoc "generationConfig" injected))
  (check injected => base))

;; 3. gemini-is-thinking-part?
(check-true (gemini-is-thinking-part? '(("text" . "thinking...") ("thought" . #t))))
(check-true (gemini-is-thinking-part? '((thought . #t) (text . "thinking..."))))
(check-false (gemini-is-thinking-part? '(("text" . "regular answer"))))
(check-false (gemini-is-thinking-part? '(("text" . "answer") ("thought" . #f))))

;; 4. 流式 thinking 状态测试：gemini-append-thinking-block! 与 gemini-stream-stop-active-block!
(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f #f))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev))))))
  ;; 追加第一段 thinking
  (gemini-append-thinking-block! stream-state "Thinking 1" "sig-1" emit)
  (check (vector-ref stream-state 5) => "thinking")
  (check (vector-ref stream-state 6) => "Thinking 1")
  (check (length (vector-ref stream-state 0)) => 1)
  (check (length events) => 2)
  (check (cdr (assoc 'type (car events))) => "thinking_start")
  (check (cdr (assoc 'type (cadr events))) => "thinking_delta")

  ;; 追加第二段 thinking
  (gemini-append-thinking-block! stream-state " and more" #f emit)
  (check (vector-ref stream-state 6) => "Thinking 1 and more")
  (check (length (vector-ref stream-state 0)) => 1)
  (check (length events) => 3)
  (check (cdr (assoc 'type (caddr events))) => "thinking_delta")

  ;; 结束 thinking block
  (gemini-stream-stop-active-block! stream-state emit)
  (check (vector-ref stream-state 5) => #f)
  (check (length events) => 4)
  (check (cdr (assoc 'type (list-ref events 3))) => "thinking_end")
  (check (cdr (assoc 'content (list-ref events 3))) => "Thinking 1 and more"))

(check-report)
