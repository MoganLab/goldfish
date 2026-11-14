;; pi-web-search-config-raw-model
;; 获取 web-search 配置记录中的原始 model 字段。
;;
;; 语法
;; ----
;; (pi-web-search-config-raw-model cfg)
;;
;; 参数
;; ----
;; cfg : pi-web-search-config?
;; web-search 配置记录。
;;
;; 返回值
;; ----
;; string | #f
;; 模型标识或 #f。

(import (scheme base) (liii check) (liii pi web-search types))

(check-set-mode! 'report-failed)

(check (pi-web-search-config-raw-model (make-pi-web-search-config "google" "gemini-2.5-flash" #f #f))
  =>
  "gemini-2.5-flash"
) ;check
(check (pi-web-search-config-raw-model (make-pi-web-search-config "google" #f #f #f)) => #f)

(check-report)
