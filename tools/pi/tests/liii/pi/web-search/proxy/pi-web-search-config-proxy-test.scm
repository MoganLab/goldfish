;; pi-web-search-config-proxy
;; 从 web-search 配置中读取代理地址。
;;
;; 语法
;; ----
;; (pi-web-search-config-proxy ws-config)
;;
;; 参数
;; ----
;; ws-config : (or pi-web-search-config? #f)
;; web-search 配置记录。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 代理地址字符串，未配置时返回 #f。

(import (liii check) (liii pi web-search types) (liii pi web-search proxy))

(check-set-mode! 'report-failed)

;; 1. proxy 字段
(check (pi-web-search-config-proxy (make-pi-web-search-config "google" #f #f "http://proxy:7890"))
  =>
  "http://proxy:7890"
) ;check

;; 2. 未配置或非记录时返回 #f
(check (pi-web-search-config-proxy (make-pi-web-search-config "google" #f #f #f)) => #f)
(check (pi-web-search-config-proxy #f) => #f)
(check (pi-web-search-config-proxy '()) => #f)

(check-report)
