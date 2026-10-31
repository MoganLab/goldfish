(import (liii check)
  (liii pi ai providers quota)
  (liii string)
  (liii pi ai providers kimi-coding)
) ;import

(check-set-mode! 'report-failed)

;; PI_KIMI_CODING_QUOTA_ERROR
;; Kimi Coding Plan 的额度耗尽识别函数。
;;
;; 语法
;; ----
;; (PI_KIMI_CODING_QUOTA_ERROR status-code body-str)
;;
;; 参数
;; ----
;; status-code : integer?
;; HTTP 状态码，只认 403 与 429。
;;
;; body-str : string?
;; 响应体原文。
;;
;; 返回值
;; ----
;; pi-quota-limit? 或 #f
;; 命中返回 record：kind 为 "quota"，code 为命中的 error.type，message 为官方消息原文。
;;
;; 注意
;; ----
;; 该服务商用 error.type 而不是 code 区分额度耗尽：exceeded_current_quota_error。
;; 普通限流是 rate_limit_error，不是额度耗尽。

(define kimi-body
  "{\"error\":{\"message\":\"You've reached your weekly (7-day) usage limit.\",\"type\":\"exceeded_current_quota_error\"}}"
) ;define

;; 1. 命中：kind=quota，code 记录 type 值，消息为官方原文
(let ((limit (PI_KIMI_CODING_QUOTA_ERROR 429 kimi-body)))
  (check (pi-quota-limit-kind limit) => "quota")
  (check (pi-quota-limit-code limit) => "exceeded_current_quota_error")
  (check (pi-quota-limit-message limit)
    =>
    "You've reached your weekly (7-day) usage limit."
  ) ;check
) ;let

;; 2. 403 也认
(check-true (and (PI_KIMI_CODING_QUOTA_ERROR 403 kimi-body) #t))

;; 3. 普通限流（type=rate_limit_error）不是额度耗尽
(check-false (PI_KIMI_CODING_QUOTA_ERROR 429
               "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"Rate limit exceeded\"}}"
             ) ;PI_KIMI_CODING_QUOTA_ERROR
) ;check-false

;; 4. 非 {403,429} 状态码不判定
(check-false (PI_KIMI_CODING_QUOTA_ERROR 400 kimi-body))

;; 5. 空体
(check-false (PI_KIMI_CODING_QUOTA_ERROR 429 ""))

(check-report)
