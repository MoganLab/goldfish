;; %make-pi-credential
;; 构造服务商 API 凭据 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-credential provider key type)
;;
;; 参数
;; ----
;; provider : string?
;; 凭据所属服务商名称
;;
;; key : string?
;; API 密钥
;;
;; type : string?
;; 凭据类型，如 api_key、oauth
;;
;; 返回值
;; ----
;; pi-credential?
;; 服务商 API 凭据 record 实例。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-credential "google" "secret-key" "api_key")))
  (check-true (pi-credential? obj))
  (check (pi-credential-provider obj) => "google")
  (check (pi-credential-key obj) => "secret-key")
  (check (pi-credential-type obj) => "api_key")
) ;let

(check-report)
