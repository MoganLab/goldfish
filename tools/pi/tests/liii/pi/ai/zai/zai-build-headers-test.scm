(import (liii check) (liii pi ai zai))

(check-set-mode! 'report-failed)

;; zai-build-headers
;; 构造发往智谱 BigModel Coding Copilot 端的 HTTP Headers。
(let ((headers (zai-build-headers "test-api-key")))
  (check (cdr (assoc "Authorization" headers)) => "Bearer test-api-key")
  (check (cdr (assoc "Content-Type" headers)) => "application/json")
) ;let

(check-report)
