(import (liii check) (liii pi ai api anthropic-messages) (liii pi ai event))

(check-set-mode! 'report-failed)

;; anthropic-map-usage
;; 映射 Anthropic 响应 usage 统计数据。
(let* ((raw-usage '(("input_tokens" . 100)
                    ("output_tokens" . 45)
                    ("cache_read_input_tokens" . 20)
                    ("cache_creation_input_tokens" . 10)))
       (usage (anthropic-map-usage raw-usage)))
  (check (cdr (assoc 'input usage)) => 100)
  (check (cdr (assoc 'output usage)) => 45)
  (check (cdr (assoc 'cacheRead usage)) => 20)
  (check (cdr (assoc 'cacheWrite usage)) => 10)
  (check (cdr (assoc 'totalTokens usage)) => 145))

(check-report)
