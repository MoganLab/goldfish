(import (liii check)
        (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; pi-resolve-web-search-provider
;; 签名：(pi-resolve-web-search-provider config opt-provider ws-config . opt-env)
;; 优先级：显式 opt > ws-config（web-search.json） > 环境变量 > config

;; 显式 opt 优先
(check (pi-resolve-web-search-provider '() "deepseek" #f) => "deepseek")
(check (pi-resolve-web-search-provider '((provider . "deepseek")) "google" #f) => "google")
;; ws-config（web-search.json，全局覆盖：任何会话 provider 都生效）
(check (pi-resolve-web-search-provider '() #f '(("provider" . "deepseek"))) => "deepseek")
(check (pi-resolve-web-search-provider '((provider . "google")) #f
              '(("provider" . "deepseek"))) => "deepseek")
(check (pi-resolve-web-search-provider '() "google"
              '(("provider" . "deepseek"))) => "google")
;; ws-config 里不认识的 provider 回退 google
(check (pi-resolve-web-search-provider '() #f '(("provider" . "openai"))) => "google")
;; config 的 web-search-provider 字段
(check (pi-resolve-web-search-provider '((web-search-provider . "deepseek")) #f #f) => "deepseek")
;; config 的 provider 字段
(check (pi-resolve-web-search-provider '((provider . "deepseek")) #f #f) => "deepseek")
;; 环境变量
(check (pi-resolve-web-search-provider '() #f #f
              '(("GF_PI_WEB_SEARCH_PROVIDER" . "deepseek"))) => "deepseek")
;; 默认 google
(check (pi-resolve-web-search-provider '() #f #f) => "google")
(check (pi-resolve-web-search-provider '((provider . "anthropic")) #f #f) => "google")

(check-report)
