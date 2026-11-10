;; pi-resolve-web-search-provider
;; 解析联网搜索使用的后端 provider。
;;
;; 语法
;; ----
;; (pi-resolve-web-search-provider config opt-provider ws-config . opt-env)
;;
;; 参数
;; ----
;; config : list?
;; 会话配置 alist。
;;
;; opt-provider : (or string? boolean?)
;; 显式传入的 provider 名称。
;;
;; ws-config : (or list? boolean?)
;; web-search.json 解析后的配置 alist。
;;
;; opt-env : (or list? boolean?)（可选）
;; 环境变量 alist（测试可控）。
;;
;; 返回值
;; ----
;; string?
;; 生效的 provider 名称。

(import (liii check) (liii pi web-search config))

(check-set-mode! 'report-failed)

;; 签名：(pi-resolve-web-search-provider config opt-provider ws-config . opt-env)
;; 优先级：显式 opt > ws-config（web-search.json） > 环境变量 > config

;; 显式 opt 优先
(check (pi-resolve-web-search-provider '() "deepseek" #f) => "deepseek")
(check (pi-resolve-web-search-provider '((provider . "deepseek")) "google" #f)
  =>
  "google"
) ;check
;; ws-config（web-search.json，全局覆盖：任何会话 provider 都生效）
(check (pi-resolve-web-search-provider '() #f '(("provider" . "deepseek")))
  =>
  "deepseek"
) ;check
(check (pi-resolve-web-search-provider '((provider . "google"))
         #f
         '(("provider" . "deepseek"))
       ) ;pi-resolve-web-search-provider
  =>
  "deepseek"
) ;check
(check (pi-resolve-web-search-provider '() "google" '(("provider" . "deepseek")))
  =>
  "google"
) ;check
;; ws-config 里不认识的 provider 回退 google
(check (pi-resolve-web-search-provider '() #f '(("provider" . "openai")))
  =>
  "google"
) ;check
;; config 的 web-search-provider 字段
(check (pi-resolve-web-search-provider '((web-search-provider . "deepseek")) #f #f)
  =>
  "deepseek"
) ;check
;; config 的 provider 字段
(check (pi-resolve-web-search-provider '((provider . "deepseek")) #f #f)
  =>
  "deepseek"
) ;check
;; 环境变量
(check (pi-resolve-web-search-provider '()
         #f
         #f
         '(("GF_PI_WEB_SEARCH_PROVIDER" . "deepseek"))
       ) ;pi-resolve-web-search-provider
  =>
  "deepseek"
) ;check
;; 默认 google
(check (pi-resolve-web-search-provider '() #f #f) => "google")
(check (pi-resolve-web-search-provider '((provider . "anthropic")) #f #f)
  =>
  "google"
) ;check

(check-report)
