;; pi-web-search-config->json
;; 将 <pi-web-search-config> record 转换为 Scheme-JSON 对象（alist）。
;;
;; 语法
;; ----
;; (pi-web-search-config->json cfg) -> alist
;;
;; 参数
;; ----
;; cfg : pi-web-search-config?
;; web-search 配置 record。
;;
;; 返回值
;; ----
;; alist
;; 可由 json->string 序列化的 JSON 对象，且可被 json->pi-web-search-config 往返还原。

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi web-search types)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? pi-web-search-config->json))

(let* ((orig (make-pi-web-search-config "deepseek"
               "deepseek-chat"
               "https://api.deepseek.com"
               "http://127.0.0.1:7890"
             ) ;make-pi-web-search-config
       ) ;orig
       (j (pi-web-search-config->json orig))
       (rt (json->pi-web-search-config (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "provider" j)) => "deepseek")
  (check (cdr (assoc "model" j)) => "deepseek-chat")
  (check (cdr (assoc "baseUrl" j)) => "https://api.deepseek.com")
  (check (cdr (assoc "httpProxy" j)) => "http://127.0.0.1:7890")
  (check-true (pi-web-search-config? rt))
  (check (pi-web-search-config-provider rt) => "deepseek")
  (check (pi-web-search-config-raw-model rt) => "deepseek-chat")
  (check (pi-web-search-config-raw-base-url rt) => "https://api.deepseek.com")
  (check (pi-web-search-config-raw-proxy rt) => "http://127.0.0.1:7890")
) ;let*

(let* ((orig (make-pi-web-search-config "google" #f #f #f))
       (j (pi-web-search-config->json orig))
      ) ;
  (check (cdr (assoc "provider" j)) => "google")
  (check (assoc "model" j) => #f)
  (check (assoc "baseUrl" j) => #f)
  (check (assoc "httpProxy" j) => #f)
) ;let*

(check-catch 'type-error (pi-web-search-config->json '(("provider" . "google"))))
(check-catch 'type-error (pi-web-search-config->json "invalid"))

(check-report)
