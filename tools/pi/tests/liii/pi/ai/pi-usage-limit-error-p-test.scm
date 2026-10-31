(import (liii check)
        (liii string)
        (liii pi ai limits))

(check-set-mode! 'report-failed)

;; pi-usage-limit-error?
;;
;; 判定条件：HTTP 状态码属于 {403, 429}，且响应体业务码在**该 provider 的码表**内。

(define zai "zai-coding-cn")

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限\"}}"
) ;define

(define five-hour-body
  "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限\"}}"
) ;define

;; 1. 429 + 1310 + zai-coding-cn：额度耗尽
(check-true (and (pi-usage-limit-error? 429 weekly-body zai) #t))

;; 2. 403 + 1308：额度耗尽（部分入口用 403）
(check-true (and (pi-usage-limit-error? 403 five-hour-body zai) #t))

;; 2b. kimi-coding 用 error.type 判定额度耗尽
(check-true (and (pi-usage-limit-error?
                   429
                   "{\"error\":{\"message\":\"You've reached your weekly (7-day) usage limit.\",\"type\":\"exceeded_current_quota_error\"}}"
                   "kimi-coding"
                 ) ;pi-usage-limit-error?
                #t
              ) ;and
) ;check-true

;; 3. 同一业务码在别的 provider 下不判定为额度耗尽
(check-false (pi-usage-limit-error? 429 weekly-body "kimi-coding"))
(check-false (pi-usage-limit-error? 429 weekly-body "openai"))

;; 4. 未提供 provider 时不判定
(check-false (pi-usage-limit-error? 429 weekly-body))

;; 5. 429 + 1302（并发槽满）：不是额度耗尽，应继续重试
(check-false (pi-usage-limit-error?
               429
               "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限\"}}"
               zai
             ) ;pi-usage-limit-error?
) ;check-false

;; 6. 429 + 普通限流文本：不是额度耗尽
(check-false (pi-usage-limit-error? 429 "Rate limit exceeded" zai))

;; 7. 状态码不在 {403, 429}：不判定为额度耗尽
(check-false (pi-usage-limit-error? 400 weekly-body zai))
(check-false (pi-usage-limit-error? 500 weekly-body zai))
(check-false (pi-usage-limit-error? 0 weekly-body zai))

;; 8. 空响应体
(check-false (pi-usage-limit-error? 429 "" zai))

(check-report)
