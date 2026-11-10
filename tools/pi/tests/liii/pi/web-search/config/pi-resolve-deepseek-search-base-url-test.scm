;; pi-resolve-deepseek-search-base-url
;; 解析 deepseek 联网搜索使用的基础 URL。
;;
;; 语法
;; ----
;; (pi-resolve-deepseek-search-base-url config opt-url . opt-env)
;;
;; 参数
;; ----
;; config : list?
;; 会话配置 alist。
;;
;; opt-url : (or string? boolean?)
;; 显式传入的基础 URL。
;;
;; opt-env : (or list? boolean?)（可选）
;; 环境变量 alist（测试可控）。
;;
;; 返回值
;; ----
;; string?
;; 生效的基础 URL 字符串。

(import (liii check) (liii pi web-search config))

(check-set-mode! 'report-failed)

;; 解析 deepseek 联网搜索使用的基础 URL。
(check (pi-resolve-deepseek-search-base-url '() #f)
  =>
  "https://api.deepseek.com"
) ;check
(check (pi-resolve-deepseek-search-base-url '() "http://localhost:9999")
  =>
  "http://localhost:9999"
) ;check
;; anthropic 兼容端点后缀要剥离
(check (pi-resolve-deepseek-search-base-url '() "https://api.deepseek.com/anthropic")
  =>
  "https://api.deepseek.com"
) ;check
(check (pi-resolve-deepseek-search-base-url '()
         #f
         '(("DEEPSEEK_BASE_URL" . "https://api.deepseek.com/anthropic"))
       ) ;pi-resolve-deepseek-search-base-url
  =>
  "https://api.deepseek.com"
) ;check
;; config 的 model-meta baseURL（provider 为 deepseek 时）
(check
  (pi-resolve-deepseek-search-base-url '((provider . "deepseek")
                                         (model-meta ("baseURL"
                                                      . "https://api.deepseek.com/anthropic")))
    #f
  ) ;pi-resolve-deepseek-search-base-url
  =>
  "https://api.deepseek.com"
) ;check

(check-report)
