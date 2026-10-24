(import (liii check) (liii pi ai zai))

(check-set-mode! 'report-failed)

;; zai-is-retriable-error?
;; 判断 ZAI 请求错误状态码是否为瞬时/可重试错误。
(check (zai-is-retriable-error? 429) => #t)
(check (zai-is-retriable-error? 500) => #t)
(check (zai-is-retriable-error? 502) => #t)
(check (zai-is-retriable-error? 503) => #t)
(check (zai-is-retriable-error? 504) => #t)
(check (zai-is-retriable-error? 400) => #f)
(check (zai-is-retriable-error? 401) => #f)

(check-report)
