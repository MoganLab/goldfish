;; pi-provider-base-url
;; 获取服务商的默认端点基础地址。
;;
;; 语法
;; ----
;; (pi-provider-base-url provider)
;;
;; 参数
;; ----
;; provider : string? 或 symbol?
;; 服务商标识符。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 服务商默认 baseUrl 字符串；若未注册则返回 #f。

(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)
(check (pi-provider-base-url "deepseek")
  =>
  "https://api.deepseek.com/anthropic"
) ;check
(check (pi-provider-base-url "google")
  =>
  "https://generativelanguage.googleapis.com/v1beta"
) ;check
(check (pi-provider-base-url "kimi-coding") => "https://api.kimi.com/coding/v1")
;; minimax 国际版 provider 已移除，未注册返回 #f
(check-false (pi-provider-base-url "minimax"))
(check (pi-provider-base-url "minimax-cn")
  =>
  "https://api.minimaxi.com/anthropic"
) ;check
(check (pi-provider-base-url "zai-coding-cn")
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4"
) ;check

;; "zai" 别名共享配置
(check (pi-provider-base-url "zai")
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4"
) ;check

;; 未注册 provider 无默认端点
(check (pi-provider-base-url "unknown-provider") => #f)

(check-report)
