;; json->pi-model-entry
;; 将 JSON 对象转换为 <pi-model-entry> record。
;;
;; 语法
;; ----
;; (json->pi-model-entry obj) -> pi-model-entry?

;;
;; 参数
;; ----
;; entry : pi-model-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi code config types)
) ;import

(check-set-mode! 'report-failed)

(let* ((j (string->json "{\"id\":\"k3.1-pro\",\"name\":\"Kimi K3.1 Pro\",\"baseUrl\":\"https://api.kimi.com\",\"api\":\"openai\",\"reasoning\":true,\"contextWindow\":1048576,\"maxTokens\":131072,\"custom\":1}"))
       (e (json->pi-model-entry j))
      ) ;
  (check-true (pi-model-entry? e))
  (check (pi-model-entry-id e) => "k3.1-pro")
  (check (pi-model-entry-name e) => "Kimi K3.1 Pro")
  (check (pi-model-entry-base-url e) => "https://api.kimi.com")
  (check (pi-model-entry-api e) => "openai")
  (check (pi-model-entry-reasoning e) => #t)
  (check (pi-model-entry-context-window e) => 1048576)
  (check (pi-model-entry-max-tokens e) => 131072)
  (check (pi-model-entry-extras e) => '(("custom" . 1)))
  (check-true (eq? (json->pi-model-entry e) e))
) ;let*

(check-catch 'type-error (json->pi-model-entry "bad"))
(check-catch 'type-error (json->pi-model-entry 123))

(check-report)
