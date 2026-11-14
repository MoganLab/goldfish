;; pi-web-search-config-base-url
;; 从 web-search 配置中读取基础 URL 地址。
;;
;; 语法
;; ----
;; (pi-web-search-config-base-url ws-config)
;;
;; 参数
;; ----
;; ws-config : (or pi-web-search-config? #f)
;; web-search 配置记录。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 基础 URL 字符串，未配置时返回 #f。

(import (liii check)
  (liii pi web-search types)
  (liii pi web-search base-url)
) ;import

(check-set-mode! 'report-failed)

;; 1. base-url 字段
(check
  (pi-web-search-config-base-url (make-pi-web-search-config "google" #f "http://custom-server:8080" #f)
  ) ;pi-web-search-config-base-url
  =>
  "http://custom-server:8080"
) ;check

;; 2. 未配置或非记录时返回 #f
(check (pi-web-search-config-base-url (make-pi-web-search-config "google" #f #f #f)) => #f)
(check (pi-web-search-config-base-url #f) => #f)
(check (pi-web-search-config-base-url '()) => #f)

(check-report)
