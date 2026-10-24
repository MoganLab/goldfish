(import (liii check) (liii pi ai kimi))

(check-set-mode! 'report-failed)

;; kimi-is-retriable-error?
(check (kimi-is-retriable-error? 429 "") => #t)
(check (kimi-is-retriable-error? 500 "") => #t)
(check (kimi-is-retriable-error? 502 "") => #t)
(check (kimi-is-retriable-error? 503 "") => #t)
(check (kimi-is-retriable-error? 0 "") => #t)
(check (kimi-is-retriable-error? 400 "") => #f)
(check (kimi-is-retriable-error? 401 "") => #f)
(check (kimi-is-retriable-error? 404 "") => #f)
(check (kimi-is-retriable-error? 200 "") => #f)

(check-report)
