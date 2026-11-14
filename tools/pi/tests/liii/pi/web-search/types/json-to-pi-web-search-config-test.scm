;; json->pi-web-search-config
;; 将 JSON 对象转换为 pi-web-search-config 记录。
;;
;; 语法
;; ----
;; (json->pi-web-search-config data)
;;
;; 参数
;; ----
;; data : pair? | pi-web-search-config? | #f
;; JSON alist 或已有配置记录。
;;
;; 返回值
;; ----
;; pi-web-search-config? | #f
;; 转换后的配置记录，输入非法时返回 #f。

(import (scheme base) (liii check) (liii pi web-search types))

(check-set-mode! 'report-failed)

(let ((cfg (json->pi-web-search-config '(("provider" . "google")
                                         ("model" . "gemini-2.5-flash")
                                         ("baseUrl" . "https://example.com")
                                         ("httpProxy" . "http://127.0.0.1:7890")
                                        )
           )
      )
     )
  (check-true (pi-web-search-config? cfg))
  (check (pi-web-search-config-provider cfg) => "google")
  (check (pi-web-search-config-raw-model cfg) => "gemini-2.5-flash")
  (check (pi-web-search-config-raw-base-url cfg) => "https://example.com")
  (check (pi-web-search-config-raw-proxy cfg) => "http://127.0.0.1:7890")
  (check-true (eq? (json->pi-web-search-config cfg) cfg))
) ;let

(let ((cfg (json->pi-web-search-config '(("provider" . "   ")
                                         ("model" . "")
                                         ("baseUrl" . "https://dash.example.com")
                                         ("httpProxy" . "http://proxy:8080")
                                        )
           )
      )
     )
  (check-true (pi-web-search-config? cfg))
  (check (pi-web-search-config-provider cfg) => #f)
  (check (pi-web-search-config-raw-model cfg) => #f)
  (check (pi-web-search-config-raw-base-url cfg) => "https://dash.example.com")
  (check (pi-web-search-config-raw-proxy cfg) => "http://proxy:8080")
) ;let

(check-false (json->pi-web-search-config #f))
(check-false (json->pi-web-search-config '()))

(check-report)
