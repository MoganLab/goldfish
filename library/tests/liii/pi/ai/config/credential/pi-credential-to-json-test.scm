;; pi-credential->json
;; 将 <pi-credential> record 转换为 Scheme-JSON 对象（alist）。
;;
;; 语法
;; ----
;; (pi-credential->json c) -> alist
;;
;; 参数
;; ----
;; c : pi-credential? | alist
;; 服务商凭据 record 或已是 JSON alist。
;;
;; 返回值
;; ----
;; alist
;; 包含 provider、type 与 key 字段的 JSON 对象。

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi ai config credential)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? pi-credential->json))

(let* ((c (pi-credential "google" "secret-key" "api_key"))
       (j (pi-credential->json c))
       (parsed (string->json (json->string j)))
      ) ;
  (check (cdr (assoc "provider" parsed)) => "google")
  (check (cdr (assoc "type" parsed)) => "api_key")
  (check (cdr (assoc "key" parsed)) => "secret-key")
) ;let*

(let ((al '(("provider" . "openai") ("type" . "api_key") ("key" . "k"))))
  (check (pi-credential->json al) => al)
) ;let

(check-report)
