(import (liii check) (liii string) (liii pi ai limits))

(check-set-mode! 'report-failed)

;; pi-usage-limit-error?
;; 判断响应是否属于「额度耗尽（不可重试）」，是 pi-detect-usage-limit 的真值形式。
;;
;; 语法
;; ----
;; (pi-usage-limit-error? status-code body-str provider)
;;
;; 参数
;; ----
;; status-code : integer?
;; HTTP 状态码，通常为 403 或 429。
;;
;; body-str : string?
;; 响应体原文。
;;
;; provider : string? 或 symbol?
;; 服务商 id；未注册的 provider 不做判定。
;;
;; 返回值
;; ----
;; pi-quota-limit? 或 #f
;; 命中返回 record（可继续用 pi-usage-limit-kind / pi-usage-limit-error-message 取值），
;; 未命中返回 #f。调用方通常只关心是否为真。
;;
;; 注意
;; ----
;; 返回 #t/#f 之外还要带上判定结果，所以返回 record 而不是布尔值。

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
(check-true (and (pi-usage-limit-error? 429
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
(check-false (pi-usage-limit-error? 429
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
