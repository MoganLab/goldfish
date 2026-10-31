(import (liii check)
        (liii string)
        (liii pi ai limits))

(check-set-mode! 'report-failed)

;; pi-detect-usage-limit
;;
;; 只按结构化业务码判定「额度耗尽」，禁止使用文本匹配：
;;   1308 -> 5 小时额度窗口耗尽（five-hour）
;;   1310 -> 每周/每月额度耗尽（weekly）
;; 1302（并发槽满）/1303、1305（短时频率）/1312（模型拥堵）都不是额度耗尽。

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}"
) ;define

(define five-hour-body
  "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限，请在 5 小时后重试。\"}}"
) ;define

;; 1. 1310：每周/每月额度耗尽
(let ((limit (pi-detect-usage-limit weekly-body)))
  (check (pi-usage-limit-kind limit) => "weekly")
  (check (cdr (assoc "code" limit)) => "1310")
  ;; 官方消息原样保留
  (check (cdr (assoc "message" limit))
    => "您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。"
  ) ;check
) ;let

;; 2. 1308：5 小时额度窗口耗尽
(let ((limit (pi-detect-usage-limit five-hour-body)))
  (check (pi-usage-limit-kind limit) => "five-hour")
  (check (cdr (assoc "code" limit)) => "1308")
) ;let

;; 3. 1302（并发槽满）不是额度耗尽
(check (pi-detect-usage-limit
         "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限，请稍后重试\"}}"
       ) ;pi-detect-usage-limit
  => #f
) ;check

;; 4. 1303 / 1305（短时频率压力）不是额度耗尽
(check (pi-detect-usage-limit
         "{\"error\":{\"code\":\"1303\",\"message\":\"您当前使用过于频繁，请稍后重试\"}}"
       ) ;pi-detect-usage-limit
  => #f
) ;check
(check (pi-detect-usage-limit
         "{\"error\":{\"code\":\"1305\",\"message\":\"请求过于频繁，请稍后重试\"}}"
       ) ;pi-detect-usage-limit
  => #f
) ;check

;; 5. 1312（模型临时拥堵）不是额度耗尽
(check (pi-detect-usage-limit
         "{\"error\":{\"code\":\"1312\",\"message\":\"当前模型访问人数过多，请稍后重试\"}}"
       ) ;pi-detect-usage-limit
  => #f
) ;check

;; 6. 兼容 code 在顶层的 envelope：{"code":1310,"msg":"..."}
(let ((limit (pi-detect-usage-limit "{\"code\":1310,\"msg\":\"每周额度已用尽\"}")))
  (check (pi-usage-limit-kind limit) => "weekly")
  (check (cdr (assoc "code" limit)) => "1310")
  (check (cdr (assoc "message" limit)) => "每周额度已用尽")
) ;let

;; 7. 兼容 Anthropic 风格 envelope：{"type":"error","error":{...,"code":"1308"}}
(let ((limit (pi-detect-usage-limit
               "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"code\":\"1308\",\"message\":\"5小时额度已用尽\"}}"
             ) ;pi-detect-usage-limit
       ) ;limit
      ) ;
  (check (pi-usage-limit-kind limit) => "five-hour")
  (check (cdr (assoc "message" limit)) => "5小时额度已用尽")
) ;let

;; 8. 纯文本响应不再按关键词判定（没有业务码就不是额度耗尽）
(check (pi-detect-usage-limit "You have exceeded your quota") => #f)
(check (pi-detect-usage-limit "您已达到5小时使用限制，请稍后再试") => #f)
(check (pi-detect-usage-limit "usage limit reached") => #f)

;; 9. 空响应体 / 空 JSON / 非对象 JSON
(check (pi-detect-usage-limit "") => #f)
(check (pi-detect-usage-limit "{}") => #f)
(check (pi-detect-usage-limit "[]") => #f)
(check (pi-detect-usage-limit #f) => #f)

;; 10. 未知业务码不是额度耗尽
(check (pi-detect-usage-limit "{\"error\":{\"code\":\"9999\",\"message\":\"x\"}}") => #f)

(check-report)
