(import (liii check) (liii pi ai config record) (liii pi ai event))

(check-set-mode! 'report-failed)

;; pi-credential
;; 构造保存 provider API 凭据的 record。
;;
;; 语法
;; ----
;; (pi-credential provider key type)
;;
;; 参数
;; ----
;; provider : string
;; 凭据所属的 provider 名称。
;; key : string
;; API 密钥。
;; type : string
;; 凭据类型，当前使用 api_key。
;;
;; 返回值
;; ----
;; pi-credential
;; 可通过访问器或 assoc 读取 provider、key 和 type 的 record。
;;
;; 说明
;; ----
;; assoc 兼容 symbol/string 键，缺失键返回 #f。
;; 普通 alist 与 credential record 混合列表仍可按 provider 查找。
;;
;; 示例
;; ----
;; (pi-credential-key (pi-credential "google" "secret" "api_key")) => "secret"

(define test-cred
  (pi-credential "google" "test-secret-key" "api_key"))

(check-true (pi-credential? test-cred))
(check (pi-credential-provider test-cred) => "google")
(check (pi-credential-key test-cred) => "test-secret-key")
(check (pi-credential-type test-cred) => "api_key")

;; assoc 兼容访问 credential record
(check (cdr (assoc 'provider test-cred)) => "google")
(check (cdr (assoc "provider" test-cred)) => "google")
(check (cdr (assoc 'key test-cred)) => "test-secret-key")
(check (cdr (assoc "key" test-cred)) => "test-secret-key")
(check (cdr (assoc 'type test-cred)) => "api_key")
(check (cdr (assoc "type" test-cred)) => "api_key")

;; 不存在的字段返回 #f
(check (assoc 'missing test-cred) => #f)

;; credential record 与普通 alist entry 混用时仍按 provider 查找
(let ((pair (assoc "google" (list (cons "other" "other-key") test-cred))))
  (check (car pair) => "google")
  (check (cdr pair) => "test-secret-key"))

(check-report)
