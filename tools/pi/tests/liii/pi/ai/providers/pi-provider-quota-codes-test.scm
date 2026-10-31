(import (liii check)
        (liii string)
        (liii pi ai providers registry))

(check-set-mode! 'report-failed)

;; pi-provider-quota-codes
;;
;; 「额度耗尽」业务码表定义在 providers/<provider>.scm 的 quotaErrorCodes 字段里；
;; 业务码是服务商私有命名空间，未配置的 provider 返回空表（不做额度判定）。

;; 1. zai-coding-cn（智谱）的额度耗尽业务码
(let ((codes (pi-provider-quota-codes "zai-coding-cn")))
  (check (cdr (assoc "1308" codes)) => "five-hour")
  (check (cdr (assoc "1310" codes)) => "weekly"))

;; 2. zai 是 zai-coding-cn 的路由别名，共享同一码表
(check (pi-provider-quota-codes "zai") => (pi-provider-quota-codes "zai-coding-cn"))

;; 3. kimi-coding 用 error.type 区分额度耗尽
(check (pi-provider-quota-codes "kimi-coding")
  => '(("exceeded_current_quota_error" . "quota"))
) ;check

;; 4. 其他 provider 未配置该字段：空表（不套用别家的业务码）
(check (pi-provider-quota-codes "openai") => '())
(check (pi-provider-quota-codes "google") => '())
(check (pi-provider-quota-codes "deepseek") => '())

;; 5. 未知 provider：空表
(check (pi-provider-quota-codes "no-such-provider") => '())

(check-report)
