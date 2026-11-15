;; pi-provider-entry->json
;; 将 <pi-provider-entry> record 转换为 Scheme-JSON 对象。
;;
;; 语法
;; ----
;; (pi-provider-entry->json entry) -> alist

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

(let* ((orig (make-pi-provider-entry "https://s"
               "Kimi"
               "https://api.kimi.com"
               "openai"
               "http://127.0.0.1:7890"
               '("k3")
               '(("extra" . "v"))
             ) ;make-pi-provider-entry
       ) ;orig
       (j (pi-provider-entry->json orig))
       (rt (json->pi-provider-entry (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "$schema" j)) => "https://s")
  (check (cdr (assoc "name" j)) => "Kimi")
  (check (cdr (assoc "models" j)) => #("k3"))
  (check (cdr (assoc "extra" j)) => "v")
  (check-true (pi-provider-entry? rt))
  (check (pi-provider-entry-name rt) => "Kimi")
  (check (pi-provider-entry-models rt) => '("k3"))
  (check (pi-provider-entry-extras rt) => '(("extra" . "v")))
) ;let*

(check-report)
