(import (liii check)
  (liii pi ai providers quota)
  (liii string)
  (liii pi ai providers zai-coding-cn)
) ;import

(check-set-mode! 'report-failed)

;; PI_ZAI_CODING_CN_QUOTA_ERROR
;; 智谱 / Z.AI GLM Coding Plan 的额度耗尽识别函数。
;;
;; 语法
;; ----
;; (PI_ZAI_CODING_CN_QUOTA_ERROR status-code body-str)
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
;; 命中返回 record：kind 为 "five-hour"（1308）或 "weekly"（1310），
;; code 为业务码，message 为官方消息原文。
;;
;; 注意
;; ----
;; 判定依据是 error.code：1308 为 5 小时额度窗口耗尽，1310 为每周/每月额度耗尽。
;; 1302（并发槽满）、1303/1305（短时频率）、1312（模型拥堵）都不是额度耗尽，
;; 返回 #f 让上层继续重试。

(define (kind limit)
  (pi-quota-limit-kind limit)
) ;define

(define (code limit)
  (pi-quota-limit-code limit)
) ;define

(define (msg limit)
  (pi-quota-limit-message limit)
) ;define

;; 1. 1310 -> weekly
(let ((limit (PI_ZAI_CODING_CN_QUOTA_ERROR 429
               "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA_ERROR
      ) ;limit
     ) ;
  (check (kind limit) => "weekly")
  (check (code limit) => "1310")
  (check (msg limit) => "您已达到每周/每月使用上限")
) ;let

;; 2. 1308 -> five-hour（403 也认）
(let ((limit (PI_ZAI_CODING_CN_QUOTA_ERROR 403
               "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA_ERROR
      ) ;limit
     ) ;
  (check (kind limit) => "five-hour")
  (check (code limit) => "1308")
) ;let

;; 3. 1302/1303/1305/1312 不是额度耗尽
(check-false (PI_ZAI_CODING_CN_QUOTA_ERROR 429
               "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA_ERROR
) ;check-false
(check-false (PI_ZAI_CODING_CN_QUOTA_ERROR 429
               "{\"error\":{\"code\":\"1303\",\"message\":\"使用过于频繁\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA_ERROR
) ;check-false
(check-false (PI_ZAI_CODING_CN_QUOTA_ERROR 429
               "{\"error\":{\"code\":\"1312\",\"message\":\"模型拥堵\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA_ERROR
) ;check-false

;; 4. 非 {403,429} 状态码不判定
(check-false (PI_ZAI_CODING_CN_QUOTA_ERROR 400
               "{\"error\":{\"code\":\"1310\",\"message\":\"x\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA_ERROR
) ;check-false
(check-false (PI_ZAI_CODING_CN_QUOTA_ERROR 500
               "{\"error\":{\"code\":\"1310\",\"message\":\"x\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA_ERROR
) ;check-false

;; 5. 非 JSON / 无业务码 / 空体
(check-false (PI_ZAI_CODING_CN_QUOTA_ERROR 429 "You have exceeded your quota"))
(check-false (PI_ZAI_CODING_CN_QUOTA_ERROR 429 "{}"))
(check-false (PI_ZAI_CODING_CN_QUOTA_ERROR 429 ""))

(check-report)
