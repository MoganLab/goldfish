(import (liii check)
  (liii pi ai providers quota)
  (liii string)
  (liii pi ai providers minimax-cn)
) ;import

(check-set-mode! 'report-failed)

;; PI_MINIMAX_CN_QUOTA_ERROR
;; MiniMax 国内版的额度耗尽识别函数。
;;
;; 语法
;; ----
;; (PI_MINIMAX_CN_QUOTA_ERROR status-code body-str)
;;
;; 返回值
;; ----
;; pi-quota-limit? 或 #f，行为与 PI_MINIMAX_QUOTA_ERROR 完全一致。

;; 2026-10-31 真实响应（api.minimaxi.com，已脱敏 request_id）
(define minimax-cn-quota-body
  "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"当前已达到 Token Plan 用量上限。为避免调用中断，请升级 Token Plan 套餐，或购买积分补充用量并开启积分自动消耗。 (2067)\"},\"request_id\":\"0735...\"}"
) ;define

;; 1. 命中：kind=five-hour，code 记录尾码
(let ((limit (PI_MINIMAX_CN_QUOTA_ERROR 429 minimax-cn-quota-body)))
  (check (pi-quota-limit-kind limit) => "five-hour")
  (check (pi-quota-limit-code limit) => "2067")
) ;let

;; 2. 与 minimax 行为一致
(check (pi-quota-limit-message (PI_MINIMAX_CN_QUOTA_ERROR 429 minimax-cn-quota-body))
  =>
  "当前已达到 Token Plan 用量上限。为避免调用中断，请升级 Token Plan 套餐，或购买积分补充用量并开启积分自动消耗。 (2067)"
) ;check

;; 3. 普通瞬时限流不是额度耗尽
(check-false (PI_MINIMAX_CN_QUOTA_ERROR 429
               "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"Rate limit exceeded\"}}"
             ) ;PI_MINIMAX_CN_QUOTA_ERROR
) ;check-false

;; 4. 非 {403,429} 状态码不判定
(check-false (PI_MINIMAX_CN_QUOTA_ERROR 400 minimax-cn-quota-body))

(check-report)
