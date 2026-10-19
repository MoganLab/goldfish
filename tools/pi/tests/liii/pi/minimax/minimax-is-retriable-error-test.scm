(import (liii check) (liii pi minimax))

(check-set-mode! 'report-failed)

;; minimax-is-retriable-error?
;; 判断 MiniMax 请求错误状态码是否为瞬时/可重试错误。
(check (minimax-is-retriable-error? 429) => #t)
(check (minimax-is-retriable-error? 500) => #t)
(check (minimax-is-retriable-error? 502) => #t)
(check (minimax-is-retriable-error? 503) => #t)
(check (minimax-is-retriable-error? 504) => #t)
(check (minimax-is-retriable-error? 400) => #f)
(check (minimax-is-retriable-error? 401) => #f)
(check (minimax-is-retriable-error? 404) => #f)

(check-report)
