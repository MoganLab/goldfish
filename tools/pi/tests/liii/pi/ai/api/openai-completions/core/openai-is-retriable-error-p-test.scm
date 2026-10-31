(import (liii check) (liii pi ai api openai-completions))

(check-set-mode! 'report-failed)

;; openai-is-retriable-error?
;; 判断 OpenAI 兼容协议的失败是否属于可重试错误。
;;
;; 语法
;; ----
;; (openai-is-retriable-error? status-code response-body-str)
;; (openai-is-retriable-error? status-code response-body-str provider)
;;
;; 参数
;; ----
;; status-code : integer?
;; HTTP 状态码；0 表示网络错误/连接失败。
;;
;; response-body-str : string?
;; 响应体原文，用于交给 provider 的额度耗尽识别函数。
;;
;; provider : string?
;; 可选，服务商 id；省略时不做额度耗尽判定。
;;
;; 返回值
;; ----
;; boolean
;; 可重试返回 #t，否则 #f。
;;
;; 注意
;; ----
;; 可重试集合是显式列表：0、408、409、429、500、502、503、504、529（501、505 等不在其中）。
;; 额度耗尽优先判为不可重试；普通 429 限流（如 rate limit）仍可重试。
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
(check (openai-is-retriable-error? 429
         "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}"
         "zai-coding-cn"
       ) ;openai-is-retriable-error?
  =>
  #f
) ;check
(check (openai-is-retriable-error? 429
         "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时用量上限，请在 5 小时后重试。\"}}"
         "zai-coding-cn"
       ) ;openai-is-retriable-error?
  =>
  #f
) ;check

;; 并发槽满（1302）/ 短时频率压力（1303、1305）/ 模型拥堵（1312）都不是额度耗尽，应继续重试
(check (openai-is-retriable-error? 429
         "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限，请稍后重试\"}}"
         "zai-coding-cn"
       ) ;openai-is-retriable-error?
  =>
  #t
) ;check
(check (openai-is-retriable-error? 429
         "{\"error\":{\"code\":\"1303\",\"message\":\"您当前使用过于频繁\"}}"
         "zai-coding-cn"
       ) ;openai-is-retriable-error?
  =>
  #t
) ;check
(check (openai-is-retriable-error? 429
         "{\"error\":{\"code\":\"1312\",\"message\":\"当前模型访问人数过多\"}}"
         "zai-coding-cn"
       ) ;openai-is-retriable-error?
  =>
  #t
) ;check

;; kimi-coding 的额度耗尽（error.type = exceeded_current_quota_error）不可重试
(check (openai-is-retriable-error? 429
         "{\"error\":{\"message\":\"You've reached your weekly (7-day) usage limit.\",\"type\":\"exceeded_current_quota_error\"}}"
         "kimi-coding"
       ) ;openai-is-retriable-error?
  =>
  #f
) ;check

;; 同一业务码在别的 provider 下不判定为额度耗尽，仍按状态码可重试
(check (openai-is-retriable-error? 429
         "{\"error\":{\"code\":\"1310\",\"message\":\"x\"}}"
         "kimi-coding"
       ) ;openai-is-retriable-error?
  =>
  #t
) ;check

;; 瞬时限流仍应重试
(check (openai-is-retriable-error? 429
         "{\"error\":{\"message\":\"Rate limit exceeded, please retry later\"}}"
       ) ;openai-is-retriable-error?
  =>
  #t
) ;check

(check-report)
