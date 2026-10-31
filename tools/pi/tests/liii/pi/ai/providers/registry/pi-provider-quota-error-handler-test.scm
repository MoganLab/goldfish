(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

;; pi-provider-quota-error-handler
;;
;; 各 provider 注册自己的额度耗尽识别函数（解析状态码 / 错误码 / JSON），
;; registry 只负责按 provider id 取出该函数；未注册时返回 #f。

;; 1. 已注册 provider 返回过程
(check-true (procedure? (pi-provider-quota-error-handler "zai-coding-cn")))
(check-true (procedure? (pi-provider-quota-error-handler "zai")))
(check-true (procedure? (pi-provider-quota-error-handler "kimi-coding")))

;; 2. 未注册 provider 返回 #f
(check-false (pi-provider-quota-error-handler "google"))
(check-false (pi-provider-quota-error-handler "openai"))
(check-false (pi-provider-quota-error-handler "no-such-provider"))

;; 3. provider 为 #f / 非字符串非符号时返回 #f
(check-false (pi-provider-quota-error-handler #f))
(check-false (pi-provider-quota-error-handler 42))

;; 4. provider id 大小写与首尾空白不敏感
(check-true (procedure? (pi-provider-quota-error-handler " ZAI-Coding-CN ")))

(check-report)
