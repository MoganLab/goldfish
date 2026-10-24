(import (liii check) (liii pi ai event) (liii pi ai kimi) (liii pi ai message))

(check-set-mode! 'report-failed)

;; kimi-map-usage
;; 将 Kimi/OpenAI 响应 usage 对象映射为标准 usage alist。
(let* ((raw '((prompt_tokens . 120)
              (completion_tokens . 45)
              (total_tokens . 165)
              (cached_tokens . 80)))
       (u (kimi-map-usage raw)))
  (check (cdr (assoc 'input u)) => 120)
  (check (cdr (assoc 'output u)) => 45)
  (check (cdr (assoc 'totalTokens u)) => 165)
  (check (cdr (assoc 'cacheRead u)) => 80)
  (check (cdr (assoc 'cacheWrite u)) => 0))

(let* ((raw '(("prompt_tokens" . 10) ("completion_tokens" . 20) ("total_tokens" . 30)))
       (u (kimi-map-usage raw)))
  (check (cdr (assoc 'input u)) => 10)
  (check (cdr (assoc 'output u)) => 20)
  (check (cdr (assoc 'totalTokens u)) => 30)
  (check (cdr (assoc 'cacheRead u)) => 0))

(check-report)
