(import (liii check) (liii pi ai api anthropic-messages))

(check-set-mode! 'report-failed)

;; anthropic-is-retriable-error?
;; 判断 Anthropic 协议的失败是否属于可重试错误。
;;
;; 语法
;; ----
;; (anthropic-is-retriable-error? status-code response-body-str)
;; (anthropic-is-retriable-error? status-code response-body-str provider)
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
;; 可重试集合：0、408、409、429、529 与所有大于等于 500 的状态码。
;; 额度耗尽（403/429 且该 provider 业务标识命中）优先判为不可重试，
;; 其余 4xx 一律不可重试。
(check (anthropic-is-retriable-error? 200 "{}") => #f)
(check (anthropic-is-retriable-error? 400 "{}") => #f)
(check (anthropic-is-retriable-error? 408 "{}") => #t)
(check (anthropic-is-retriable-error? 409 "{}") => #t)
(check (anthropic-is-retriable-error? 429 "{}") => #t)
(check (anthropic-is-retriable-error? 500 "{}") => #t)
(check (anthropic-is-retriable-error? 503 "{}") => #t)
(check (anthropic-is-retriable-error? 0 "") => #t)

;; 额度耗尽（5 小时窗口 / 周额度）属于账户级限额，不应重试
(check (anthropic-is-retriable-error? 429
         "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}"
         "zai-coding-cn"
       ) ;anthropic-is-retriable-error?
  =>
  #f
) ;check
(check (anthropic-is-retriable-error? 429
         "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限，请在 5 小时后重试。\"}}"
         "zai-coding-cn"
       ) ;anthropic-is-retriable-error?
  =>
  #f
) ;check

;; 并发槽满（1302）/ 短时频率压力（1303、1305）/ 模型拥堵（1312）都不是额度耗尽，应继续重试
(check (anthropic-is-retriable-error? 429
         "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限，请稍后重试\"}}"
         "zai-coding-cn"
       ) ;anthropic-is-retriable-error?
  =>
  #t
) ;check
(check (anthropic-is-retriable-error? 429
         "{\"error\":{\"code\":\"1303\",\"message\":\"您当前使用过于频繁\"}}"
         "zai-coding-cn"
       ) ;anthropic-is-retriable-error?
  =>
  #t
) ;check
(check (anthropic-is-retriable-error? 429
         "{\"error\":{\"code\":\"1312\",\"message\":\"当前模型访问人数过多\"}}"
         "zai-coding-cn"
       ) ;anthropic-is-retriable-error?
  =>
  #t
) ;check

;; 同一业务码在别的 provider 下不判定为额度耗尽，仍按状态码可重试
(check (anthropic-is-retriable-error? 429
         "{\"error\":{\"code\":\"1310\",\"message\":\"x\"}}"
         "kimi-coding"
       ) ;anthropic-is-retriable-error?
  =>
  #t
) ;check

;; 瞬时限流仍应重试
(check (anthropic-is-retriable-error? 429
         "{\"error\":{\"message\":\"Rate limit exceeded, please retry later\"}}"
       ) ;anthropic-is-retriable-error?
  =>
  #t
) ;check

(check-report)
