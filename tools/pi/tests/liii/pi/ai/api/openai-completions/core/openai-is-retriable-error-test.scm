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

;; 额度耗尽（5 小时窗口 / 周额度）属于账户级限额，重试无意义
(check (openai-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}")
       => #f)
(check (openai-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时用量上限，请在 5 小时后重试。\"}}")
       => #f)

;; 并发槽满（1302）/ 短时频率压力（1303、1305）/ 模型拥堵（1312）都不是额度耗尽，应继续重试
(check (openai-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限，请稍后重试\"}}")
       => #t)
(check (openai-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1303\",\"message\":\"您当前使用过于频繁\"}}")
       => #t)
(check (openai-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1312\",\"message\":\"当前模型访问人数过多\"}}")
       => #t)

;; 瞬时限流仍应重试
(check (openai-is-retriable-error?
         429
         "{\"error\":{\"message\":\"Rate limit exceeded, please retry later\"}}")
       => #t)

(check-report)
