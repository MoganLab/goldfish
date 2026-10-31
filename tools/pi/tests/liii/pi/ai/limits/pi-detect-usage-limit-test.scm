(import (liii check)
  (liii pi ai providers quota)
  (liii string)
  (liii pi ai limits)
) ;import

(check-set-mode! 'report-failed)

;; pi-detect-usage-limit
;; 按 provider 注册的处理函数识别响应体是否属于「服务商额度耗尽」。
;;
;; 语法
;; ----
;; (pi-detect-usage-limit status-code body-str provider)
;;
;; 参数
;; ----
;; status-code : integer?
;; HTTP 状态码，原样交给该 provider 的处理函数判定（通常为 403 或 429）。
;;
;; body-str : string?
;; 响应体原文；非字符串时直接返回 #f。
;;
;; provider : string? 或 symbol?
;; 服务商 id（如 "zai-coding-cn"、"kimi-coding"），用于取该家注册的判定函数；
;; 未注册的 provider 不做判定。
;;
;; 返回值
;; ----
;; pi-quota-limit? 或 #f
;; 命中时返回 record（kind / code / message）；服务商未给消息时以 HTTP 状态码兜底。
;;
;; 注意
;; ----
;; 不做任何文本匹配：错误消息文案会变、会被本地化，只按结构化标识判定。
;; 业务码是各家私有命名空间，不会跨 provider 套用（如智谱 1308/1310 只对 zai-coding-cn 生效）。
;; 智谱 1302（并发槽满）/1303、1305（短时频率）/1312（模型拥堵）不是额度耗尽。

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
    =>
    "您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。"
  ) ;check
) ;let

;; 2. 1308：5 小时额度窗口耗尽
(let ((limit (pi-detect-usage-limit 429 five-hour-body zai)))
  (check (pi-usage-limit-kind limit) => "five-hour")
  (check (pi-quota-limit-code limit) => "1308")
) ;let

;; 3. provider 大小写/空白不敏感
(check (pi-usage-limit-kind (pi-detect-usage-limit 429 weekly-body " ZAI-Coding-CN "))
  =>
  "weekly"
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
  =>
  #f
) ;check

;; 7. 1303 / 1305（短时频率压力）不是额度耗尽
(check (pi-detect-usage-limit 429
         "{\"error\":{\"code\":\"1303\",\"message\":\"您当前使用过于频繁，请稍后重试\"}}"
         zai
       ) ;pi-detect-usage-limit
  =>
  #f
) ;check
(check (pi-detect-usage-limit 429
         "{\"error\":{\"code\":\"1305\",\"message\":\"请求过于频繁，请稍后重试\"}}"
         zai
       ) ;pi-detect-usage-limit
  =>
  #f
) ;check

;; 8. 1312（模型临时拥堵）不是额度耗尽
(check (pi-detect-usage-limit 429
         "{\"error\":{\"code\":\"1312\",\"message\":\"当前模型访问人数过多，请稍后重试\"}}"
         zai
       ) ;pi-detect-usage-limit
  =>
  #f
) ;check

;; 9. 兼容 code 在顶层的 envelope：{"code":1310,"msg":"..."}
(let ((limit (pi-detect-usage-limit 429
               "{\"code\":1310,\"msg\":\"每周额度已用尽\"}"
               zai
             ) ;pi-detect-usage-limit
      ) ;limit
     ) ;
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
    =>
    "You've reached your weekly (7-day) usage limit."
  ) ;check
) ;let

;; 12. kimi 的 type 在别的 provider 下不生效
(check (pi-detect-usage-limit 429 kimi-weekly-body "zai-coding-cn") => #f)

;; 13. 纯文本响应不再按关键词判定（没有业务码就不是额度耗尽）
(check (pi-detect-usage-limit 429 "You have exceeded your quota" zai) => #f)
(check (pi-detect-usage-limit 429
         "您已达到5小时使用限制，请稍后再试"
         zai
       ) ;pi-detect-usage-limit
  =>
  #f
) ;check
(check (pi-detect-usage-limit 429 "usage limit reached" zai) => #f)

;; 14. 空响应体 / 空 JSON / 非对象 JSON
(check (pi-detect-usage-limit 429 "" zai) => #f)
(check (pi-detect-usage-limit 429 "{}" zai) => #f)
(check (pi-detect-usage-limit 429 "[]" zai) => #f)
(check (pi-detect-usage-limit 429 #f zai) => #f)

;; 15. 未知业务码不是额度耗尽
(check (pi-detect-usage-limit 429
         "{\"error\":{\"code\":\"9999\",\"message\":\"x\"}}"
         zai
       ) ;pi-detect-usage-limit
  =>
  #f
) ;check

(check-report)
