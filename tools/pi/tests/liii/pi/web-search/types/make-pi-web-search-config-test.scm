;; make-pi-web-search-config
;; 构造 web-search 配置记录。
;;
;; 语法
;; ----
;; (make-pi-web-search-config provider model base-url proxy)
;;
;; 参数
;; ----
;; provider : string | #f
;; 搜索后端名称。
;; model : string | #f
;; 模型标识。
;; base-url : string | #f
;; 自定义基础 URL。
;; proxy : string | #f
;; 代理 URL。
;;
;; 返回值
;; ----
;; pi-web-search-config?
;; web-search 配置记录。

(import (scheme base) (liii check) (liii pi web-search types))

(check-set-mode! 'report-failed)

(let ((cfg (make-pi-web-search-config "deepseek" "deepseek-chat" "https://api.deepseek.com" "http://127.0.0.1:7890")))
  (check-true (pi-web-search-config? cfg))
  (check-false (pi-web-search-config? '()))
  (check (pi-web-search-config-provider cfg) => "deepseek")
  (check (pi-web-search-config-raw-model cfg) => "deepseek-chat")
  (check (pi-web-search-config-raw-base-url cfg) => "https://api.deepseek.com")
  (check (pi-web-search-config-raw-proxy cfg) => "http://127.0.0.1:7890")
) ;let

(check-report)
