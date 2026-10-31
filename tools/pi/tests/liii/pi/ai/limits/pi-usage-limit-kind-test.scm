(import (liii check) (liii pi ai providers quota) (liii pi ai limits))

(check-set-mode! 'report-failed)

;; pi-usage-limit-kind
;; 取额度耗尽判定结果里的额度窗口类型。
;;
;; 语法
;; ----
;; (pi-usage-limit-kind limit)
;;
;; 参数
;; ----
;; limit : pi-quota-limit?
;; 额度耗尽判定结果。
;;
;; 返回值
;; ----
;; string 或 #f
;; 额度窗口类型由各 provider 定义，如 "five-hour"（5 小时窗口）、"weekly"（每周/每月）、
;; "quota"（只有额度语义、无窗口细分）；不是 record 时返回 #f。
;;
;; 注意
;; ----
;; 该值只用于日志与展示（日志里的 limit 字段），不参与重试决策。

;; 1. 正常取值
(check (pi-usage-limit-kind (%make-pi-quota-limit "weekly" "1310" "x"))
  =>
  "weekly"
) ;check
(check (pi-usage-limit-kind (%make-pi-quota-limit "five-hour" "1308" "x"))
  =>
  "five-hour"
) ;check
(check (pi-usage-limit-kind (%make-pi-quota-limit "quota" "exceeded_current_quota_error" #f)
       ) ;pi-usage-limit-kind
  =>
  "quota"
) ;check

;; 2. 非额度耗尽结果返回 #f
(check (pi-usage-limit-kind #f) => #f)
(check (pi-usage-limit-kind '()) => #f)

;; 3. 与真实 provider 识别结果一致
(check (pi-usage-limit-kind (pi-detect-usage-limit 429
                              "{\"error\":{\"code\":\"1310\",\"message\":\"每周额度已用尽\"}}"
                              "zai-coding-cn"
                            ) ;pi-detect-usage-limit
       ) ;pi-usage-limit-kind
  =>
  "weekly"
) ;check

(check-report)
