(import (liii check)
        (liii pi ai message)
        (liii pi ai api anthropic-messages thinking))

(check-set-mode! 'report-failed)

;; 1. anthropic-thinking-level->budget（对齐上游 pi DEFAULT_THINKING_BUDGETS）
(check (anthropic-thinking-level->budget "minimal") => 1024)
(check (anthropic-thinking-level->budget "low") => 2048)
(check (anthropic-thinking-level->budget "medium") => 8192)
(check (anthropic-thinking-level->budget "high") => 16384)
;; xhigh/max 钳制到 high
(check (anthropic-thinking-level->budget "xhigh") => 16384)
(check (anthropic-thinking-level->budget "max") => 16384)
(check (anthropic-thinking-level->budget "off") => #f)
(check (anthropic-thinking-level->budget "unknown") => #f)

;; 2. anthropic-inject-thinking
;; off 时不注入
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "off")))
  (check-false (assoc "thinking" res)))

;; low 时注入 thinking 并保持 max_tokens (8192 > 2048)
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "low")))
  (check-true (pair? (assoc "thinking" res)))
  (let ((th (cdr (assoc "thinking" res))))
    (check (cdr (assoc "type" th)) => "enabled")
    (check (cdr (assoc "budget_tokens" th)) => 2048))
  (check (cdr (assoc "max_tokens" res)) => 8192))

;; high 时 budget 是 16384，若原 max_tokens 是 8192 <= 16384，自动提升到 budget + 4096
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "high")))
  (check (cdr (assoc "max_tokens" res)) => (+ 16384 4096)))

;; 3. 流式 thinking 状态机：anthropic-handle-thinking-delta 与 anthropic-finish-thinking-block
(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f))
       (events '())
       (emit (lambda (ev) (set! events (append events (list ev))))))
  ;; 初始化 thinking block 接收状态 (index 6 记录 thinking 对象)
  (vector-set! stream-state 6 '((thinking . "") (signature . "") (index . 0)))

  ;; delta 1
  (anthropic-handle-thinking-delta '(("type" . "thinking_delta") ("thinking" . "Step 1")) 0 stream-state emit)
  (check (length events) => 1)
  (check (cdr (assoc 'type (car events))) => "thinking_delta")
  (check (cdr (assoc 'delta (car events))) => "Step 1")

  ;; delta 2 (signature)
  (anthropic-handle-thinking-delta '(("type" . "signature_delta") ("signature" . "sig123")) 0 stream-state emit)
  (check (length events) => 1) ;; signature 增量不 emit thinking_delta

  ;; finish block
  (anthropic-finish-thinking-block stream-state emit)
  (check (length events) => 2)
  (check (cdr (assoc 'type (cadr events))) => "thinking_end")
  (check (cdr (assoc 'content (cadr events))) => "Step 1")
  (check (length (vector-ref stream-state 0)) => 1)
  (let ((blk (car (vector-ref stream-state 0))))
    (check-true (pi-thinking-block? blk))
    (check (pi-thinking-block-thinking blk) => "Step 1")
    (check (pi-thinking-block-thinking-signature blk) => "sig123")))

(check-report)
