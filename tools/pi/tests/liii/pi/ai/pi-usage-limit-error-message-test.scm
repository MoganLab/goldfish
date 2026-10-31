(import (liii check)
  (liii pi ai providers quota)
  (liii string)
  (liii pi ai limits)
) ;import

(check-set-mode! 'report-failed)

;; pi-usage-limit-error-message
;;
;; 面向用户的消息 = "provider: " + 服务商官方消息原文（不改写、不追加建议）。

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}"
) ;define

(define five-hour-body
  "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限，请在 5 小时后重试。\"}}"
) ;define

;; 1. 带 provider：前缀 + 官方消息原文
(let ((limit (pi-detect-usage-limit 429 weekly-body "zai-coding-cn")))
  (check (pi-usage-limit-error-message limit "zai-coding-cn")
    =>
    "zai-coding-cn: 您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。"
  ) ;check
) ;let

;; 2. 不带 provider：只有官方消息原文
(check (pi-usage-limit-error-message (pi-detect-usage-limit 429 five-hour-body "zai-coding-cn")
       ) ;pi-usage-limit-error-message
  =>
  "您已达到5小时使用上限，请在 5 小时后重试。"
) ;check

;; 3. provider 为空串或 #f 时不加前缀
(let ((limit (pi-detect-usage-limit 429 five-hour-body "zai-coding-cn")))
  (check (pi-usage-limit-error-message limit "")
    =>
    "您已达到5小时使用上限，请在 5 小时后重试。"
  ) ;check
  (check (pi-usage-limit-error-message limit #f)
    =>
    "您已达到5小时使用上限，请在 5 小时后重试。"
  ) ;check
) ;let

;; 4. 官方消息缺失时退回 HTTP 状态（状态码由 pi-usage-limit-error? 记录）
(let ((limit (pi-usage-limit-error? 429 "{\"error\":{\"code\":\"1310\"}}" "zai-coding-cn")
      ) ;limit
     ) ;
  (check (pi-usage-limit-error-message limit "zai-coding-cn")
    =>
    "zai-coding-cn: HTTP 429"
  ) ;check
) ;let

;; 5. 非额度耗尽输入返回 #f
(check (pi-usage-limit-error-message #f "zai-coding-cn") => #f)

(check-report)
