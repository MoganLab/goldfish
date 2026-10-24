(import (liii check) (liii pi ai config record) (liii pi ai event))

(check-set-mode! 'report-failed)

;; pi-auth-record-test.scm
;; 验证 pi-credential record 构造器、谓词与访问器，以及对 assoc 的平滑兼容。

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

(check-report)
