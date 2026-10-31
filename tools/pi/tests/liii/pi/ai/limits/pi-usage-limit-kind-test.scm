(import (liii check) (liii pi ai providers quota) (liii pi ai limits))

(check-set-mode! 'report-failed)

;; pi-usage-limit-kind
;;
;; 取额度窗口类型（five-hour / weekly / quota，由各 provider 决定）。

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
