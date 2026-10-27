(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

(check (gemini-is-retriable-error? 200 "{}") => #f)
(check (gemini-is-retriable-error? 400 "{}") => #f)
(check (gemini-is-retriable-error? 408 "{}") => #t)
(check (gemini-is-retriable-error? 409 "{}") => #t)
(check (gemini-is-retriable-error? 429 "{}") => #t)
(check (gemini-is-retriable-error? 500 "{}") => #t)
(check (gemini-is-retriable-error? 503 "{}") => #t)
(check (gemini-is-retriable-error? 0 "") => #t)

(check-report)
