;; pi-web-search-config-raw-proxy
;; 获取 web-search 配置记录中的原始 proxy 字段。
;;
;; 语法
;; ----
;; (pi-web-search-config-raw-proxy cfg)
;;
;; 参数
;; ----
;; cfg : pi-web-search-config?
;; web-search 配置记录。
;;
;; 返回值
;; ----
;; string | #f
;; 代理 URL 或 #f。

(import (scheme base) (liii check) (liii pi web-search types))

(check-set-mode! 'report-failed)

(check (pi-web-search-config-raw-proxy (make-pi-web-search-config "google" #f #f "http://127.0.0.1:7890"))
  =>
  "http://127.0.0.1:7890"
) ;check
(check (pi-web-search-config-raw-proxy (make-pi-web-search-config "google" #f #f #f)) => #f)

(check-report)
