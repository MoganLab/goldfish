(import (liii check) (liii pi deepseek))

(check-set-mode! 'report-failed)

;; deepseek-is-retriable-error?
;; 判断 deepseek 请求错误状态码是否为瞬时/可重试错误。
(check (deepseek-is-retriable-error? 429) => #t)
(check (deepseek-is-retriable-error? 500) => #t)
(check (deepseek-is-retriable-error? 502) => #t)
(check (deepseek-is-retriable-error? 503) => #t)
(check (deepseek-is-retriable-error? 504) => #t)
(check (deepseek-is-retriable-error? 400) => #f)
(check (deepseek-is-retriable-error? 401) => #f)
(check (deepseek-is-retriable-error? 404) => #f)

(check-report)
