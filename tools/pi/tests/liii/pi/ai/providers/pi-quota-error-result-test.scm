(import (liii check) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

;; pi-quota-error-result
;;
;; provider 可复用的默认实现：状态码 ∈ {403, 429} 且业务标识命中码表时，
;; 返回 <pi-quota-limit>，否则 #f。

(define table '(("1308" . "five-hour") ("1310" . "weekly")))

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"每周额度已用尽\"}}"
) ;define

;; 1. 命中：429
(let ((limit (pi-quota-error-result 429 weekly-body table)))
  (check (pi-quota-limit-kind limit) => "weekly")
  (check (pi-quota-limit-code limit) => "1310")
  (check (pi-quota-limit-message limit) => "每周额度已用尽")
) ;let

;; 2. 命中：403（部分入口用 403 表示额度耗尽）
(check-true (pi-quota-limit? (pi-quota-error-result 403 weekly-body table)))

;; 3. 状态码不在 {403, 429}：不判定
(check-false (pi-quota-error-result 400 weekly-body table))
(check-false (pi-quota-error-result 500 weekly-body table))
(check-false (pi-quota-error-result 0 weekly-body table))

;; 4. 标识不在码表：不判定
(check-false (pi-quota-error-result 429
               "{\"error\":{\"code\":\"1302\",\"message\":\"x\"}}"
               table
             ) ;pi-quota-error-result
) ;check-false

;; 5. 空响应体 / 非 JSON：不判定
(check-false (pi-quota-error-result 429 "" table))
(check-false (pi-quota-error-result 429 "quota exceeded" table))

(check-report)
