(import (liii check)
        (liii string)
        (liii pi ai providers registry))

(check-set-mode! 'report-failed)

;; pi-provider-quota-error-handler / pi-provider-quota-error
;;
;; 额度耗尽识别由各 provider 注册自己的处理函数（解析 JSON / 错误码 / 状态码），
;; registry 只负责按 provider id 取函数；未注册的 provider 返回 #f。

;; 1. 已注册 provider 返回过程
(check-true (procedure? (pi-provider-quota-error-handler "zai-coding-cn")))
(check-true (procedure? (pi-provider-quota-error-handler "zai")))
(check-true (procedure? (pi-provider-quota-error-handler "kimi-coding")))

;; 2. 未注册 provider 返回 #f
(check-false (pi-provider-quota-error-handler "google"))
(check-false (pi-provider-quota-error-handler "openai"))
(check-false (pi-provider-quota-error-handler "no-such-provider"))
(check-false (pi-provider-quota-error-handler #f))

;; 3. 通过 registry 调用：zai 别名与 zai-coding-cn 行为一致
(check (pi-provider-quota-error 429
         "{\"error\":{\"code\":\"1310\",\"message\":\"每周额度已用尽\"}}"
         "zai")
  => (pi-provider-quota-error 429
       "{\"error\":{\"code\":\"1310\",\"message\":\"每周额度已用尽\"}}"
       "zai-coding-cn"
     ) ;pi-provider-quota-error
) ;check

;; 4. 未注册 provider 不做判定
(check-false (pi-provider-quota-error 429
               "{\"error\":{\"code\":\"1310\",\"message\":\"x\"}}"
               "google"
             ) ;pi-provider-quota-error
) ;check-false

;; 5. provider 为 #f 时不做判定
(check-false (pi-provider-quota-error 429
               "{\"error\":{\"code\":\"1310\",\"message\":\"x\"}}"
             ) ;pi-provider-quota-error
) ;check-false

(check-report)
