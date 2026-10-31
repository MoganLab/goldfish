(import (liii check)
        (liii pi ai providers quota)
        (liii string)
        (liii pi ai providers kimi-coding))

(check-set-mode! 'report-failed)

;; PI_KIMI_CODING_QUOTA_ERROR
;;
;; Kimi Coding Plan：HTTP 403/429 + error.type = exceeded_current_quota_error
;; （该服务商用 type 而不是 code 区分额度耗尽）

(define kimi-body
  "{\"error\":{\"message\":\"You've reached your weekly (7-day) usage limit.\",\"type\":\"exceeded_current_quota_error\"}}"
) ;define

;; 1. 命中：kind=quota，code 记录 type 值，消息为官方原文
(let ((limit (PI_KIMI_CODING_QUOTA_ERROR 429 kimi-body)))
  (check (pi-quota-limit-kind limit) => "quota")
  (check (pi-quota-limit-code limit) => "exceeded_current_quota_error")
  (check (pi-quota-limit-message limit)
    => "You've reached your weekly (7-day) usage limit."
  ) ;check
) ;let

;; 2. 403 也认
(check-true (and (PI_KIMI_CODING_QUOTA_ERROR 403 kimi-body) #t))

;; 3. 普通限流（type=rate_limit_error）不是额度耗尽
(check-false (PI_KIMI_CODING_QUOTA_ERROR
               429
               "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"Rate limit exceeded\"}}"
             ) ;PI_KIMI_CODING_QUOTA_ERROR
) ;check-false

;; 4. 非 {403,429} 状态码不判定
(check-false (PI_KIMI_CODING_QUOTA_ERROR 400 kimi-body))

;; 5. 空体
(check-false (PI_KIMI_CODING_QUOTA_ERROR 429 ""))

(check-report)
