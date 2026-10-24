(import (liii check) (liii pi ai event) (liii pi ai gemini))

(check-set-mode! 'report-failed)

;; gemini-map-usage
;; 映射 Gemini usageMetadata 到 pi agent 的 usage 格式。
(let* ((raw-u '(("promptTokenCount" . 100) ("candidatesTokenCount" . 50)
                ("thoughtsTokenCount" . 20) ("cachedContentTokenCount" . 30)
                ("totalTokenCount" . 170)))
       (u (gemini-map-usage raw-u)))
  (check (cdr (assoc 'input u)) => 70)
  (check (cdr (assoc 'output u)) => 70)
  (check (cdr (assoc 'cacheRead u)) => 30)
  (check (cdr (assoc 'cacheWrite u)) => 0)
  (check (cdr (assoc 'totalTokens u)) => 170))

(check-report)
