(import (liii check)
        (liii string)
        (liii pi ai providers zai-coding-cn))

(check-set-mode! 'report-failed)

;; PI_ZAI_CODING_CN_QUOTA-ERROR
;;
;; 智谱 / Z.AI GLM Coding Plan：HTTP 403/429 + error.code ∈ {1308, 1310}
;;   1308 -> 5 小时额度窗口耗尽（five-hour）
;;   1310 -> 每周/每月额度耗尽（weekly）
;; 1302/1303/1305/1312 是并发/频率/拥堵，不是额度耗尽（返回 #f）。

(define (kind limit) (cdr (assoc "kind" limit)))
(define (code limit) (cdr (assoc "code" limit)))
(define (msg limit) (cdr (assoc "message" limit)))

;; 1. 1310 -> weekly
(let ((limit (PI_ZAI_CODING_CN_QUOTA-ERROR
               429
               "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA-ERROR
       ) ;limit
      ) ;
  (check (kind limit) => "weekly")
  (check (code limit) => "1310")
  (check (msg limit) => "您已达到每周/每月使用上限")
) ;let

;; 2. 1308 -> five-hour（403 也认）
(let ((limit (PI_ZAI_CODING_CN_QUOTA-ERROR
               403
               "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA-ERROR
       ) ;limit
      ) ;
  (check (kind limit) => "five-hour")
  (check (code limit) => "1308")
) ;let

;; 3. 1302/1303/1305/1312 不是额度耗尽
(check-false (PI_ZAI_CODING_CN_QUOTA-ERROR
               429
               "{\"error\":{\"code\":\"1302\",\"message\":\"当前并发数已达到上限\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA-ERROR
) ;check-false
(check-false (PI_ZAI_CODING_CN_QUOTA-ERROR
               429
               "{\"error\":{\"code\":\"1303\",\"message\":\"使用过于频繁\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA-ERROR
) ;check-false
(check-false (PI_ZAI_CODING_CN_QUOTA-ERROR
               429
               "{\"error\":{\"code\":\"1312\",\"message\":\"模型拥堵\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA-ERROR
) ;check-false

;; 4. 非 {403,429} 状态码不判定
(check-false (PI_ZAI_CODING_CN_QUOTA-ERROR
               400
               "{\"error\":{\"code\":\"1310\",\"message\":\"x\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA-ERROR
) ;check-false
(check-false (PI_ZAI_CODING_CN_QUOTA-ERROR
               500
               "{\"error\":{\"code\":\"1310\",\"message\":\"x\"}}"
             ) ;PI_ZAI_CODING_CN_QUOTA-ERROR
) ;check-false

;; 5. 非 JSON / 无业务码 / 空体
(check-false (PI_ZAI_CODING_CN_QUOTA-ERROR 429 "You have exceeded your quota"))
(check-false (PI_ZAI_CODING_CN_QUOTA-ERROR 429 "{}"))
(check-false (PI_ZAI_CODING_CN_QUOTA-ERROR 429 ""))

(check-report)
