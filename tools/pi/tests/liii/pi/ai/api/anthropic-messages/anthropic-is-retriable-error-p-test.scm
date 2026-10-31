(import (liii check) (liii pi ai api anthropic-messages))

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

;; 额度耗尽（5 小时窗口 / 周额度）属于账户级限额，不应重试
(check (anthropic-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}")
       => #f)
(check (anthropic-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1302\",\"message\":\"您已达到5小时使用上限，请在 5 小时后重试。\"}}")
       => #f)

;; 瞬时限流仍应重试
(check (anthropic-is-retriable-error?
         429
         "{\"error\":{\"message\":\"Rate limit exceeded, please retry later\"}}")
       => #t)

(check-report)
