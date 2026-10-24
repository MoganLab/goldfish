(import (liii check) (liii pi ai anthropic))

(check-set-mode! 'report-failed)

;; anthropic-is-retriable-error?
;; 判断 Anthropic 请求状态码或错误体是否为可重试错误。
(check (anthropic-is-retriable-error? 200 "{}") => #f)
(check (anthropic-is-retriable-error? 400 "{}") => #f)
(check (anthropic-is-retriable-error? 408 "{}") => #t)
(check (anthropic-is-retriable-error? 409 "{}") => #t)
(check (anthropic-is-retriable-error? 429 "{}") => #t)
(check (anthropic-is-retriable-error? 500 "{}") => #t)
(check (anthropic-is-retriable-error? 503 "{}") => #t)
(check (anthropic-is-retriable-error? 0 "") => #t)

(check-report)
