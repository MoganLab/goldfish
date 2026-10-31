(import (liii check)
  (liii pi ai providers quota)
  (liii pi ai providers registry)
) ;import

(check-set-mode! 'report-failed)

;; pi-provider-quota-error
;;
;; 调用各 provider 注册的额度耗尽识别函数；未注册的 provider 不做判定（返回 #f），
;; 处理函数抛异常时也返回 #f（不影响请求主流程）。

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"每周额度已用尽\"}}"
) ;define

;; 1. zai 别名与 zai-coding-cn 行为一致
(check (pi-provider-quota-error 429 weekly-body "zai")
  =>
  (pi-provider-quota-error 429 weekly-body "zai-coding-cn")
) ;check

;; 2. 命中时返回 <pi-quota-limit>
(check-true (pi-quota-limit? (pi-provider-quota-error 429 weekly-body "zai-coding-cn"))
) ;check-true

;; 3. 未注册 provider 不做判定
(check-false (pi-provider-quota-error 429 weekly-body "google"))

;; 4. provider 为 #f 时不做判定
(check-false (pi-provider-quota-error 429 weekly-body))

;; 5. 空响应体不做判定
(check-false (pi-provider-quota-error 429 "" "zai-coding-cn"))

(check-report)
