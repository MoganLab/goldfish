(import (liii check) (liii pi ai retry))

(check-set-mode! 'report-failed)

;; pi-is-retriable-error?
;; 通用可重试判定：状态码之外还需看错误体，
;; 额度耗尽（5 小时窗口 / 周额度）属于账户级限额，不应重试。

(check (pi-is-retriable-error? 408 "") => #t)
(check (pi-is-retriable-error? 409 "") => #t)
(check (pi-is-retriable-error? 429 "") => #t)
(check (pi-is-retriable-error? 500 "") => #t)
(check (pi-is-retriable-error? 0 "") => #t)
(check (pi-is-retriable-error? 400 "") => #f)

;; 额度耗尽：不可重试
(check (pi-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}"
         "zai-coding-cn")
       => #f)
(check (pi-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限，请在 5 小时后重试。\"}}"
         "zai-coding-cn")
       => #f)

;; 并发槽满（1302）/ 短时频率压力（1303、1305）/ 模型拥堵（1312）都不是额度耗尽，应继续重试
(check (pi-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限，请稍后重试\"}}"
         "zai-coding-cn")
       => #t)
(check (pi-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1303\",\"message\":\"您当前使用过于频繁\"}}"
         "zai-coding-cn")
       => #t)
(check (pi-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1312\",\"message\":\"当前模型访问人数过多\"}}"
         "zai-coding-cn")
       => #t)

;; 同一业务码在别的 provider 下不判定为额度耗尽，仍按状态码可重试
(check (pi-is-retriable-error?
         429
         "{\"error\":{\"code\":\"1310\",\"message\":\"x\"}}"
         "kimi-coding")
       => #t)

;; 瞬时限流：可重试
(check (pi-is-retriable-error?
         429
         "{\"error\":{\"message\":\"Rate limit exceeded, please retry later\"}}")
       => #t)

(check-report)
