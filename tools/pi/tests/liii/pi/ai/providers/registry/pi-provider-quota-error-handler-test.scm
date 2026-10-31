(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

;; pi-provider-quota-error-handler
;; 取某个 provider 注册的额度耗尽识别函数。
;;
;; 语法
;; ----
;; (pi-provider-quota-error-handler provider)
;;
;; 参数
;; ----
;; provider : string? 或 symbol?
;; 服务商 id，大小写与首尾空白不敏感（如 " ZAI-Coding-CN "）。
;;
;; 返回值
;; ----
;; procedure 或 #f
;; 已注册时返回处理函数（签名 (handler status-code body-str)）；未注册返回 #f。
;;
;; 注意
;; ----
;; 处理函数由各 provider 模块导出（PI_ZAI_CODING_CN_QUOTA_ERROR、
;; PI_KIMI_CODING_QUOTA_ERROR 等），registry 只做按 id 查找。

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
