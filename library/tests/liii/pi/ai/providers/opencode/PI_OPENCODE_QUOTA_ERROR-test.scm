;; PI_OPENCODE_QUOTA_ERROR
;; OpenCode Go 订阅的额度耗尽识别函数。
;;
;; 语法
;; ----
;; (PI_OPENCODE_QUOTA_ERROR status-code body-str)
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
;; 命中返回 record：kind 为 "quota"，code 为命中的错误标识，message 为官方消息原文。
;;
;; 注意
;; ----
;; 该服务商用 GoUsageLimitError、go-limit 或 FreeUsageLimitError 标识额度耗尽。
;; 普通限流不是额度耗尽。

(import (liii check)
  (liii pi ai providers quota)
  (liii string)
  (liii pi ai providers opencode)
) ;import

(check-set-mode! 'report-failed)

(define go-body
  "{\"type\":\"error\",\"error\":{\"type\":\"go-limit\",\"message\":\"Usage limit reached. It will reset in 2 hours.\",\"metadata\":{\"limitName\":\"5-hour\"}}}"
) ;define

(define go-named-body
  "{\"error\":{\"name\":\"GoUsageLimitError\",\"message\":\"5-hour limit reached\"}}"
) ;define

;; 1. 命中 go-limit：kind=quota，code 记录 type 值，消息为官方原文
(let ((limit (PI_OPENCODE_QUOTA_ERROR 429 go-body)))
  (check (pi-quota-limit-kind limit) => "quota")
  (check (pi-quota-limit-code limit) => "go-limit")
  (check (pi-quota-limit-message limit)
    =>
    "Usage limit reached. It will reset in 2 hours."
  ) ;check
) ;let

;; 2. 命中 GoUsageLimitError
(let ((limit (PI_OPENCODE_QUOTA_ERROR 429 go-named-body)))
  (check (pi-quota-limit-kind limit) => "quota")
  (check (pi-quota-limit-code limit) => "GoUsageLimitError")
  (check (pi-quota-limit-message limit) => "5-hour limit reached")
) ;let

;; 3. 403 也认
(check-true (and (PI_OPENCODE_QUOTA_ERROR 403 go-body) #t))

;; 4. 普通限流不是额度耗尽
(check-false (PI_OPENCODE_QUOTA_ERROR 429
               "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"Rate limit exceeded\"}}"
             ) ;PI_OPENCODE_QUOTA_ERROR
) ;check-false

;; 5. 非 {403,429} 状态码不判定
(check-false (PI_OPENCODE_QUOTA_ERROR 400 go-body))

;; 6. 空体
(check-false (PI_OPENCODE_QUOTA_ERROR 429 ""))

(check-report)
