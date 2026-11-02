;; PI_SUPPORTED_PROVIDERS
;; 预定义的 provider 探测与路由优先级列表。
;;
;; 语法
;; ----
;; PI_SUPPORTED_PROVIDERS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 支持的服务商名称字符串列表。

(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

(check-true (list? PI_SUPPORTED_PROVIDERS))
(check (car PI_SUPPORTED_PROVIDERS) => "google")
(check (if (member "kimi-coding" PI_SUPPORTED_PROVIDERS) #t #f) => #t)
(check (if (member "anthropic" PI_SUPPORTED_PROVIDERS) #t #f) => #f)
(check (if (member "glm" PI_SUPPORTED_PROVIDERS) #t #f) => #f)
(check (if (member "deepseek" PI_SUPPORTED_PROVIDERS) #t #f) => #t)
(check (if (member "minimax-cn" PI_SUPPORTED_PROVIDERS) #t #f) => #t)
(check (if (member "minimax" PI_SUPPORTED_PROVIDERS) #t #f) => #f)
(check (if (member "zai-coding-cn" PI_SUPPORTED_PROVIDERS) #t #f) => #t)

(check-report)
