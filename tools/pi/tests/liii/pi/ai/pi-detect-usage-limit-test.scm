(import (liii check) (liii string) (liii pi ai limits))

(check-set-mode! 'report-failed)

;; pi-detect-usage-limit
;; 识别服务商"额度耗尽"类错误（HTTP 429/403），区分 5 小时窗口额度与周/月额度，
;; 与瞬时限流（rate limit）区分开。

(define zai-weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}")

(define zai-five-hour-body
  "{\"error\":{\"code\":\"1302\",\"message\":\"您已达到5小时用量上限，请在 5 小时后重试。\"}}")

;; 1. 周/月额度耗尽
(let ((limit (pi-detect-usage-limit zai-weekly-body)))
  (check (pi-usage-limit-kind limit) => "weekly")
  (check (pi-usage-limit-reset limit) => "2026-11-05 13:12:35"))

;; 2. 5 小时窗口额度耗尽
(let ((limit (pi-detect-usage-limit zai-five-hour-body)))
  (check (pi-usage-limit-kind limit) => "five-hour"))

;; 3. 英文 5 小时额度耗尽
(let ((limit (pi-detect-usage-limit
               "{\"error\":{\"message\":\"Your API key has reached the 5-hour usage limit\"}}")))
  (check (pi-usage-limit-kind limit) => "five-hour"))

;; 4. 瞬时限流不算额度耗尽（应继续重试）
(check (pi-detect-usage-limit
         "{\"error\":{\"message\":\"Rate limit exceeded, please retry later\"}}")
       => #f)

;; 5. 空响应体
(check (pi-detect-usage-limit "") => #f)
(check (pi-detect-usage-limit "{}") => #f)

;; 6. 非 JSON 文本中的额度耗尽
(let ((limit (pi-detect-usage-limit "You have exceeded your quota")))
  (check (pi-usage-limit-kind limit) => "quota"))

;; 7. 非 JSON 文本中的 5 小时额度耗尽
(let ((limit (pi-detect-usage-limit "您已达到5小时使用限制，请稍后再试")))
  (check (pi-usage-limit-kind limit) => "five-hour"))

;; 8. 纯文本 "usage limit reached" 也应识别为额度耗尽
(let ((limit (pi-detect-usage-limit "usage limit reached")))
  (check (pi-usage-limit-kind limit) => "quota"))

;; 9. Gemini 风格的按分钟限流属于瞬时限流，应继续重试
(check (pi-detect-usage-limit
         "{\"error\":{\"code\":429,\"message\":\"Quota exceeded for quota metric 'Generate Content API requests per minute'\"}}")
       => #f)

;; 10. Gemini 风格的按日配额耗尽属于额度耗尽
(let ((limit (pi-detect-usage-limit
               "{\"error\":{\"code\":429,\"message\":\"Quota exceeded for quota metric 'Generate Content API requests per day'\"}}")))
  (check (pi-usage-limit-kind limit) => "daily"))

;; pi-usage-limit-error?
;; 只有 429/403 才可能是额度耗尽，且需命中上限关键词
(check (pi-usage-limit-error? 429 zai-weekly-body) => #t)
(check (pi-usage-limit-error? 429 zai-five-hour-body) => #t)
(check (pi-usage-limit-error? 429 "{\"error\":{\"message\":\"Rate limit exceeded\"}}") => #f)
(check (pi-usage-limit-error? 500 zai-weekly-body) => #f)
(check (pi-usage-limit-error? 429 "") => #f)

;; pi-usage-limit-error-message
;; 提示信息需说明额度类型、重置时间，并保留原始信息
(let ((msg (pi-usage-limit-error-message (pi-detect-usage-limit zai-weekly-body) "zai-coding-cn")))
  (check (string-contains? msg "zai-coding-cn") => #t)
  (check (string-contains? msg "每周/每月额度已用尽") => #t)
  (check (string-contains? msg "2026-11-05 13:12:35") => #t))

(let ((msg (pi-usage-limit-error-message (pi-detect-usage-limit zai-five-hour-body))))
  (check (string-contains? msg "5 小时额度已用尽") => #t))
