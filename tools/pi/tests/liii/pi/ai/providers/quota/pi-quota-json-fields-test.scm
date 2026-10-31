(import (liii check) (liii string) (liii pi ai providers quota))

(check-set-mode! 'report-failed)

;; pi-quota-json-fields
;;
;; 从错误响应体提取 (候选标识列表 . 官方消息)，兼容各家不同的字段结构。

;; 1. 智谱：error.code + error.message
(let ((fields (pi-quota-json-fields "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限\"}}"
              ) ;pi-quota-json-fields
      ) ;fields
     ) ;
  (check (car fields) => '("1310"))
  (check (cdr fields) => "您已达到每周/每月使用上限")
) ;let

;; 2. 顶层 code + msg（数字型 code）
(let ((fields (pi-quota-json-fields "{\"code\":1310,\"msg\":\"每周额度已用尽\"}")
      ) ;fields
     ) ;
  (check (car fields) => '("1310"))
  (check (cdr fields) => "每周额度已用尽")
) ;let

;; 3. kimi：error.type（无 code 字段）
(let ((fields (pi-quota-json-fields "{\"error\":{\"type\":\"exceeded_current_quota_error\",\"message\":\"quota\"}}"
              ) ;pi-quota-json-fields
      ) ;fields
     ) ;
  (check (car fields) => '("exceeded_current_quota_error"))
  (check (cdr fields) => "quota")
) ;let

;; 4. code 与 type 同时存在时按优先级都收集（code 在前）
(let ((fields (pi-quota-json-fields "{\"error\":{\"code\":\"1308\",\"type\":\"rate_limit_error\",\"message\":\"x\"}}"
              ) ;pi-quota-json-fields
      ) ;fields
     ) ;
  (check (car fields) => '("1308" "rate_limit_error"))
) ;let

;; 5. 非 JSON / 无对象：空候选 + #f 消息
(check (car (pi-quota-json-fields "You have exceeded your quota")) => '())
(check (cdr (pi-quota-json-fields "You have exceeded your quota")) => #f)
(check (car (pi-quota-json-fields "")) => '())
(check (car (pi-quota-json-fields "[]")) => '())

(check-report)
