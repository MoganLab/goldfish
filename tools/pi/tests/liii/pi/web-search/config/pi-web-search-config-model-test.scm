;; pi-web-search-config-model
;; 从 web-search 配置中解析联网搜索使用的模型标识。
;;
;; 语法
;; ----
;; (pi-web-search-config-model ws-config)
;;
;; 参数
;; ----
;; ws-config : list?
;; web-search.json 解析后的配置 alist。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 模型标识字符串，未配置时返回 #f。

(import (liii check)
        (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; 从 web-search 配置中解析联网搜索使用的模型标识。
(check (pi-web-search-config-model
         '(("provider" . "google") ("model" . "gemini-2.5-pro"))) => "gemini-2.5-pro")
(check (pi-web-search-config-model '(("provider" . "deepseek"))) => #f)

(check-report)
