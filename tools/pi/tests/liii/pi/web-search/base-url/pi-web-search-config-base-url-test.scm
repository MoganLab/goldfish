;; pi-web-search-config-base-url
;; 从 web-search 配置中读取基础 URL 地址。
;;
;; 语法
;; ----
;; (pi-web-search-config-base-url ws-config)
;;
;; 参数
;; ----
;; ws-config : list?
;; web-search.json 解析后的配置 alist。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 基础 URL 字符串，未配置时返回 #f。

(import (liii check) (liii pi web-search base-url))

(check-set-mode! 'report-failed)

;; 1. baseUrl 字段
(check (pi-web-search-config-base-url '(("baseUrl" . "http://custom-server:8080")))
  =>
  "http://custom-server:8080"
) ;check

;; 2. baseURL 字段
(check (pi-web-search-config-base-url '(("baseURL" . "http://custom-server-caps:8080")))
  =>
  "http://custom-server-caps:8080"
) ;check

;; 3. base-url 字段
(check (pi-web-search-config-base-url '(("base-url" . "http://custom-server-dash:8080")))
  =>
  "http://custom-server-dash:8080"
) ;check

;; 4. 未配置或空串时返回 #f
(check (pi-web-search-config-base-url '()) => #f)
(check (pi-web-search-config-base-url '(("baseUrl" . "   "))) => #f)

(check-report)
