;; pi-web-search-config-proxy
;; 从 web-search 配置中读取代理地址。
;;
;; 语法
;; ----
;; (pi-web-search-config-proxy ws-config)
;;
;; 参数
;; ----
;; ws-config : list?
;; web-search.json 解析后的配置 alist。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 代理地址字符串，未配置时返回 #f。

(import (liii check) (liii pi web-search proxy))

(check-set-mode! 'report-failed)

;; 1. httpProxy 字段
(check (pi-web-search-config-proxy '(("httpProxy" . "http://proxy:7890")))
  =>
  "http://proxy:7890"
) ;check

;; 2. proxy 字段
(check (pi-web-search-config-proxy '(("proxy" . "http://proxy-alt:7890")))
  =>
  "http://proxy-alt:7890"
) ;check

;; 3. http-proxy 字段
(check (pi-web-search-config-proxy '(("http-proxy" . "http://proxy-dash:7890")))
  =>
  "http://proxy-dash:7890"
) ;check

;; 4. 未配置或空串时返回 #f
(check (pi-web-search-config-proxy '()) => #f)
(check (pi-web-search-config-proxy '(("httpProxy" . "   "))) => #f)

(check-report)
