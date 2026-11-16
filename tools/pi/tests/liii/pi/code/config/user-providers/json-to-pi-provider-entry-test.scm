;; json->pi-provider-entry
;; 将 JSON 对象转换为 <pi-provider-entry> record。
;;
;; 语法
;; ----
;; (json->pi-provider-entry obj) -> pi-provider-entry?

;;
;; 参数
;; ----
;; entry : pi-provider-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi code config user-providers)
) ;import

(check-set-mode! 'report-failed)

(let* ((j (string->json "{\"$schema\":\"https://s\",\"name\":\"Kimi\",\"baseUrl\":\"https://api.kimi.com\",\"api\":\"openai\",\"httpProxy\":\"http://127.0.0.1:7890\",\"models\":[\"k3\"],\"extraKey\":123}"))
       (e (json->pi-provider-entry j))
      ) ;
  (check-true (pi-provider-entry? e))
  (check (pi-provider-entry-schema e) => "https://s")
  (check (pi-provider-entry-name e) => "Kimi")
  (check (pi-provider-entry-base-url e) => "https://api.kimi.com")
  (check (pi-provider-entry-api e) => "openai")
  (check (pi-provider-entry-http-proxy e) => "http://127.0.0.1:7890")
  (check (pi-provider-entry-models e) => '("k3"))
  (check (pi-provider-entry-extras e) => '(("extraKey" . 123)))
  (check-true (eq? (json->pi-provider-entry e) e))
) ;let*

(check-catch 'type-error (json->pi-provider-entry "not-object"))
(check-catch 'type-error (json->pi-provider-entry 123))

(check-report)
