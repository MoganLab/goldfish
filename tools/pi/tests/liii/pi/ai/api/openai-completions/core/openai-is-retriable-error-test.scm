(import (liii check) (liii pi ai api openai-completions))

(check-set-mode! 'report-failed)

;; openai-is-retriable-error?
(check (openai-is-retriable-error? 429 "") => #t)
(check (openai-is-retriable-error? 500 "") => #t)
(check (openai-is-retriable-error? 502 "") => #t)
(check (openai-is-retriable-error? 503 "") => #t)
(check (openai-is-retriable-error? 0 "") => #t)
(check (openai-is-retriable-error? 400 "") => #f)
(check (openai-is-retriable-error? 401 "") => #f)
(check (openai-is-retriable-error? 404 "") => #f)
(check (openai-is-retriable-error? 200 "") => #f)

(check-report)
