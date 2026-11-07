;; pi-resolve-search-base-url
;; 解析联网搜索使用的基础 URL。
;;
;; 语法
;; ----
;; (pi-resolve-search-base-url config opt-url)
;;
;; 参数
;; ----
;; config : list?
;; 会话配置 alist，可包含 provider 与 model-meta 字段。
;;
;; opt-url : (or string? boolean?)
;; 显式传入的基础 URL。
;;
;; 返回值
;; ----
;; string?
;; 生效的基础 URL 字符串，未配置时回退 Google 官方地址。

(import (liii check)
        (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; 1. 显式传入的基础 URL 优先
(check (pi-resolve-search-base-url '() "http://localhost:9999") => "http://localhost:9999")

;; 2. provider 为 google 时读取 model-meta 中的 base-url
(check (pi-resolve-search-base-url
         '((provider . "google")
           (model-meta (base-url . "http://meta-base:8080")))
         #f
       ) ;pi-resolve-search-base-url
  => "http://meta-base:8080"
) ;check

;; 3. 未配置时回退默认地址
(check (pi-resolve-search-base-url '() #f)
  => "https://generativelanguage.googleapis.com/v1beta"
) ;check

(check-report)
