(import (liii check) (liii pi ai api anthropic-messages thinking))

(check-set-mode! 'report-failed)

;; off 时不注入
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "off"))
      ) ;
  (check-false (assoc "thinking" res))
) ;let*

;; low 时注入 thinking 并保持 max_tokens (8192 > 2048)
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "low"))
      ) ;
  (check-true (pair? (assoc "thinking" res)))
  (let ((th (cdr (assoc "thinking" res))))
    (check (cdr (assoc "type" th)) => "enabled")
    (check (cdr (assoc "budget_tokens" th)) => 2048)
  ) ;let
  (check (cdr (assoc "max_tokens" res)) => 8192)
) ;let*

;; high 时自动提升过小的 max_tokens
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "high"))
      ) ;
  (check (cdr (assoc "max_tokens" res)) => (+ 16384 4096))
) ;let*

(check-report)
