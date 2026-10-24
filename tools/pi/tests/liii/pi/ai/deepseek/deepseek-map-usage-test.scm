(import (liii check) (liii pi ai deepseek))

(check-set-mode! 'report-failed)

;; deepseek-map-usage
;; 将 DeepSeek/Anthropic 兼容网关 usage 映射为标准 usage alist。
(let* ((raw '((input_tokens . 150)
              (output_tokens . 60)
              (cache_read_input_tokens . 100)
              (cache_creation_input_tokens . 20)))
       (u (deepseek-map-usage raw)))
  (check (cdr (assoc 'input u)) => 150)
  (check (cdr (assoc 'output u)) => 60)
  (check (cdr (assoc 'totalTokens u)) => 210)
  (check (cdr (assoc 'cacheRead u)) => 100)
  (check (cdr (assoc 'cacheWrite u)) => 20))

(let* ((raw '(("input_tokens" . 10) ("output_tokens" . 20)))
       (u (deepseek-map-usage raw)))
  (check (cdr (assoc 'input u)) => 10)
  (check (cdr (assoc 'output u)) => 20)
  (check (cdr (assoc 'totalTokens u)) => 30)
  (check (cdr (assoc 'cacheRead u)) => 0)
  (check (cdr (assoc 'cacheWrite u)) => 0))

(check-report)
