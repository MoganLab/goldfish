(import (liii check)
        (liii pi ai providers quota)
        (liii string)
        (liii pi ai limits))

(check-set-mode! 'report-failed)

;; pi-detect-usage-limit
;;
;; 只按结构化业务码判定「额度耗尽」，禁止使用文本匹配；
;; 业务码是各家私有命名空间，因此必须结合 provider 判定。
;;
;; zai-coding-cn（智谱 / Z.AI GLM Coding Plan）：
;;   1308 -> 5 小时额度窗口耗尽（five-hour）
;;   1310 -> 每周/每月额度耗尽（weekly）
;;   1302/1303/1305/1312 都不是额度耗尽

(define zai "zai-coding-cn")

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}"
) ;define

(define five-hour-body
  "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限，请在 5 小时后重试。\"}}"
) ;define

;; 1. 1310：每周/每月额度耗尽
(let ((limit (pi-detect-usage-limit 429 weekly-body zai)))
  (check (pi-usage-limit-kind limit) => "weekly")
  (check (pi-quota-limit-code limit) => "1310")
  ;; 官方消息原样保留
  (check (pi-quota-limit-message limit)
    => "您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。"
  ) ;check
) ;let

;; 2. 1308：5 小时额度窗口耗尽
(let ((limit (pi-detect-usage-limit 429 five-hour-body zai)))
  (check (pi-usage-limit-kind limit) => "five-hour")
  (check (pi-quota-limit-code limit) => "1308")
) ;let

;; 3. provider 大小写/空白不敏感
(check (pi-usage-limit-kind (pi-detect-usage-limit 429 weekly-body " ZAI-Coding-CN "))
  => "weekly"
) ;check

;; 4. 同一业务码在别的 provider 下不生效（命名空间不通用）
(check (pi-detect-usage-limit 429 weekly-body "kimi-coding") => #f)
(check (pi-detect-usage-limit 429 weekly-body "openai") => #f)
(check (pi-detect-usage-limit 429 five-hour-body "deepseek") => #f)

;; 5. 未提供 provider 时不判定额度
(check (pi-detect-usage-limit 429 weekly-body) => #f)

;; 6. 1302（并发槽满）不是额度耗尽
(check (pi-detect-usage-limit 429
         "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限，请稍后重试\"}}"
         zai
       ) ;pi-detect-usage-limit
  => #f
) ;check

;; 7. 1303 / 1305（短时频率压力）不是额度耗尽
(check (pi-detect-usage-limit 429
         "{\"error\":{\"code\":\"1303\",\"message\":\"您当前使用过于频繁，请稍后重试\"}}"
         zai
       ) ;pi-detect-usage-limit
  => #f
) ;check
(check (pi-detect-usage-limit 429
         "{\"error\":{\"code\":\"1305\",\"message\":\"请求过于频繁，请稍后重试\"}}"
         zai
       ) ;pi-detect-usage-limit
  => #f
) ;check

;; 8. 1312（模型临时拥堵）不是额度耗尽
(check (pi-detect-usage-limit 429
         "{\"error\":{\"code\":\"1312\",\"message\":\"当前模型访问人数过多，请稍后重试\"}}"
         zai
       ) ;pi-detect-usage-limit
  => #f
) ;check

;; 9. 兼容 code 在顶层的 envelope：{"code":1310,"msg":"..."}
(let ((limit (pi-detect-usage-limit 429 "{\"code\":1310,\"msg\":\"每周额度已用尽\"}" zai)))
  (check (pi-usage-limit-kind limit) => "weekly")
  (check (pi-quota-limit-code limit) => "1310")
  (check (pi-quota-limit-message limit) => "每周额度已用尽")
) ;let

;; 10. 兼容 Anthropic 风格 envelope：{"type":"error","error":{...,"code":"1308"}}
(let ((limit (pi-detect-usage-limit 429
               "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"code\":\"1308\",\"message\":\"5小时额度已用尽\"}}"
               zai
             ) ;pi-detect-usage-limit
       ) ;limit
      ) ;
  (check (pi-usage-limit-kind limit) => "five-hour")
  (check (pi-quota-limit-message limit) => "5小时额度已用尽")
) ;let

(define kimi-weekly-body
  "{\"error\":{\"message\":\"You've reached your weekly (7-day) usage limit.\",\"type\":\"exceeded_current_quota_error\"}}"
) ;define

;; 11. kimi-coding 用 error.type 区分额度耗尽（没有 code 字段）
(let ((limit (pi-detect-usage-limit 429 kimi-weekly-body "kimi-coding")))
  (check (pi-usage-limit-kind limit) => "quota")
  (check (pi-quota-limit-code limit) => "exceeded_current_quota_error")
  (check (pi-quota-limit-message limit)
    => "You've reached your weekly (7-day) usage limit."
  ) ;check
) ;let

;; 12. kimi 的 type 在别的 provider 下不生效
(check (pi-detect-usage-limit 429 kimi-weekly-body "zai-coding-cn") => #f)

;; 13. 纯文本响应不再按关键词判定（没有业务码就不是额度耗尽）
(check (pi-detect-usage-limit 429 "You have exceeded your quota" zai) => #f)
(check (pi-detect-usage-limit 429 "您已达到5小时使用限制，请稍后再试" zai) => #f)
(check (pi-detect-usage-limit 429 "usage limit reached" zai) => #f)

;; 14. 空响应体 / 空 JSON / 非对象 JSON
(check (pi-detect-usage-limit 429 "" zai) => #f)
(check (pi-detect-usage-limit 429 "{}" zai) => #f)
(check (pi-detect-usage-limit 429 "[]" zai) => #f)
(check (pi-detect-usage-limit 429 #f zai) => #f)

;; 15. 未知业务码不是额度耗尽
(check (pi-detect-usage-limit 429 "{\"error\":{\"code\":\"9999\",\"message\":\"x\"}}" zai)
  => #f
) ;check

(check-report)
