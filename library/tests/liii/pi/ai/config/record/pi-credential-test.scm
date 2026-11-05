;; pi-credential
;; 构造服务商 API 凭据 record 对象。
;;
;; 语法
;; ----
;; (pi-credential provider key type)
;;
;; 参数
;; ----
;; provider : string?
;; 服务商名称。
;;
;; key : string?
;; API 密钥。
;;
;; type : string?
;; 凭据类型。
;;
;; 返回值
;; ----
;; pi-credential?
;; 凭据 record 实例。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(let ((c (pi-credential "google" "secret-key" "api_key")))
  (check-true (pi-credential? c))
  (check (pi-credential-provider c) => "google")
  (check (pi-credential-key c) => "secret-key")
  (check (pi-credential-type c) => "api_key")
) ;let

(check-report)
