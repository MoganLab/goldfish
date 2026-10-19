(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-thinking-block
;; 构造 thinking 类型的 content block record 对象。
(let* ((blk (pi-thinking-block "Let me think...")))
  ;; record 谓词与访问器
  (check-true (pi-thinking-block? blk))
  (check-true (pi-block? blk))
  (check (pi-block-type blk) => "thinking")
  (check (pi-thinking-block-thinking blk) => "Let me think...")
  (check (pi-thinking-block-thinking-signature blk) => #f)

  ;; 兼容 alist 访问
  (check (cdr (assoc "type" blk)) => "thinking")
  (check (cdr (assoc 'type blk)) => "thinking")
  (check (cdr (assoc "thinking" blk)) => "Let me think...")
  (check (cdr (assoc 'thinking blk)) => "Let me think..."))

;; 带 thinkingSignature 的 thinking block
(let* ((blk (pi-thinking-block "Reasoning step" "sig-xyz-123")))
  (check-true (pi-thinking-block? blk))
  (check (pi-thinking-block-thinking blk) => "Reasoning step")
  (check (pi-thinking-block-thinking-signature blk) => "sig-xyz-123")
  (check (cdr (assoc "thinkingSignature" blk)) => "sig-xyz-123"))

(check-report)
