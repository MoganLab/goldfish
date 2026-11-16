;; pi-model-entry->json
;; 将 <pi-model-entry> record 转换为 Scheme-JSON 对象。
;;
;; 语法
;; ----
;; (pi-model-entry->json entry) -> alist

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

(let* ((orig (make-pi-model-entry "k3.1-pro"
               "Kimi K3.1 Pro"
               "https://api.kimi.com"
               "openai"
               #f
               262144
               32768
               '(("tag" . "x"))
             ) ;make-pi-model-entry
       ) ;orig
       (j (pi-model-entry->json orig))
       (rt (json->pi-model-entry (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "id" j)) => "k3.1-pro")
  (check (cdr (assoc "reasoning" j)) => 'false)
  (check (cdr (assoc "contextWindow" j)) => 262144)
  (check (cdr (assoc "maxTokens" j)) => 32768)
  (check (cdr (assoc "tag" j)) => "x")
  (check-true (pi-model-entry? rt))
  (check (pi-model-entry-id rt) => "k3.1-pro")
  (check (pi-model-entry-reasoning rt) => #f)
  (check (pi-model-entry-context-window rt) => 262144)
  (check (pi-model-entry-extras rt) => '(("tag" . "x")))
) ;let*

(check-catch 'type-error (pi-model-entry->json '(("id" . "x"))))
(check-catch 'type-error (pi-model-entry->json "invalid"))

(check-report)
