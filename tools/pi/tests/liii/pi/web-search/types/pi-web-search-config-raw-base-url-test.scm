;; pi-web-search-config-raw-base-url
;; 获取 web-search 配置记录中的原始 base-url 字段。
;;
;; 语法
;; ----
;; (pi-web-search-config-raw-base-url cfg)
;;
;; 参数
;; ----
;; cfg : pi-web-search-config?
;; web-search 配置记录。
;;
;; 返回值
;; ----
;; string | #f
;; 基础 URL 或 #f。

(import (scheme base) (liii check) (liii pi web-search types))

(check-set-mode! 'report-failed)

(check (pi-web-search-config-raw-base-url (make-pi-web-search-config "google" #f "https://example.com" #f))
  =>
  "https://example.com"
) ;check
(check (pi-web-search-config-raw-base-url (make-pi-web-search-config "google" #f #f #f)) => #f)

(check-report)
