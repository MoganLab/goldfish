(import (liii check)
        (liii pi models builtin))

(check-set-mode! 'report-failed)

;; PI_SUPPORTED_PROVIDERS
;; 预定义的 provider 探测与路由优先级列表

(check-true (list? PI_SUPPORTED_PROVIDERS))
(check (car PI_SUPPORTED_PROVIDERS) => "google")
(check (if (member "kimi-coding" PI_SUPPORTED_PROVIDERS) #t #f) => #t)
(check (if (member "anthropic" PI_SUPPORTED_PROVIDERS) #t #f) => #t)
(check (if (member "glm" PI_SUPPORTED_PROVIDERS) #t #f) => #t)
(check (if (member "deepseek" PI_SUPPORTED_PROVIDERS) #t #f) => #t)
(check (if (member "minimax" PI_SUPPORTED_PROVIDERS) #t #f) => #t)
(check (if (member "minimax-cn" PI_SUPPORTED_PROVIDERS) #t #f) => #t)
(check (if (member "zai-coding-cn" PI_SUPPORTED_PROVIDERS) #t #f) => #t)

(check-report)
