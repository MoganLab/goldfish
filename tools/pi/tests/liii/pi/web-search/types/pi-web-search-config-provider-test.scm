;; pi-web-search-config-provider
;; 获取 web-search 配置记录中的 provider 字段。
;;
;; 语法
;; ----
;; (pi-web-search-config-provider cfg)
;;
;; 参数
;; ----
;; cfg : pi-web-search-config?
;; web-search 配置记录。
;;
;; 返回值
;; ----
;; string | #f
;; 搜索后端名称。

(import (scheme base) (liii check) (liii pi web-search types))

(check-set-mode! 'report-failed)

(check (pi-web-search-config-provider (make-pi-web-search-config "deepseek" #f #f #f)) => "deepseek")
(check (pi-web-search-config-provider (make-pi-web-search-config #f #f #f #f)) => #f)

(check-report)
