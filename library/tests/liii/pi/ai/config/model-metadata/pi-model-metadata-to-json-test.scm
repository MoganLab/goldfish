;; pi-model-metadata->json
;; 将 <pi-model-metadata> record 转换为 Scheme-JSON 对象（alist）。
;;
;; 语法
;; ----
;; (pi-model-metadata->json m) -> alist
;;
;; 参数
;; ----
;; m : pi-model-metadata?
;; 模型元数据 record。
;;
;; 返回值
;; ----
;; alist
;; 可由 json->string 序列化的 JSON 对象。

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi ai config model-metadata)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? pi-model-metadata->json))

(let* ((m (pi-model-metadata "gemini-3.8-flash"
            "Gemini 3.8 Flash"
            "google"
            "https://generativelanguage.googleapis.com"
            1000000
            8192
            #t
          ) ;pi-model-metadata
       ) ;m
       (j (pi-model-metadata->json m))
       (parsed (string->json (json->string j)))
      ) ;
  (check (cdr (assoc "id" parsed)) => "gemini-3.8-flash")
  (check (cdr (assoc "name" parsed)) => "Gemini 3.8 Flash")
  (check (cdr (assoc "provider" parsed)) => "google")
  (check (cdr (assoc "baseUrl" parsed)) => "https://generativelanguage.googleapis.com")
  (check (cdr (assoc "matched" parsed)) => 'true)
  (check (cdr (assoc "contextWindow" parsed)) => 1000000)
  (check (cdr (assoc "maxTokens" parsed)) => 8192)
) ;let*

(let* ((m (pi-model-metadata "custom" "Custom" 'openai "https://api.openai.com" #f #f #f))
       (j (pi-model-metadata->json m))
      ) ;
  (check (cdr (assoc "provider" j)) => "openai")
  (check (cdr (assoc "matched" j)) => 'false)
  (check (assoc "contextWindow" j) => #f)
  (check (assoc "maxTokens" j) => #f)
) ;let*

(check-catch 'type-error (pi-model-metadata->json '(("id" . "m"))))
(check-catch 'type-error (pi-model-metadata->json "invalid"))

(check-report)
