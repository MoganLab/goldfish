(import (liii check)
        (liii string)
        (liii pi ai limits))

(check-set-mode! 'report-failed)

;; pi-usage-limit-error?
;;
;; 判定条件：HTTP 状态码属于 {403, 429}，且响应体业务码命中额度耗尽码表。

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限\"}}"
) ;define

(define five-hour-body
  "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限\"}}"
) ;define

;; 1. 429 + 1310：额度耗尽
(check-true (and (pi-usage-limit-error? 429 weekly-body) #t))

;; 2. 403 + 1308：额度耗尽（部分入口用 403）
(check-true (and (pi-usage-limit-error? 403 five-hour-body) #t))

;; 3. 429 + 1302（并发槽满）：不是额度耗尽，应继续重试
(check-false (pi-usage-limit-error?
               429
               "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限\"}}"
             ) ;pi-usage-limit-error?
) ;check-false

;; 4. 429 + 普通限流文本：不是额度耗尽
(check-false (pi-usage-limit-error? 429 "Rate limit exceeded"))

;; 5. 状态码不在 {403, 429}：不判定为额度耗尽
(check-false (pi-usage-limit-error? 400 weekly-body))
(check-false (pi-usage-limit-error? 500 weekly-body))
(check-false (pi-usage-limit-error? 0 weekly-body))

;; 6. 空响应体
(check-false (pi-usage-limit-error? 429 ""))

(check-report)
