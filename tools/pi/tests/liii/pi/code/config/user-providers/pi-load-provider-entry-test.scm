;; pi-load-provider-entry
;; 读取 models/<provider>.json 并解析为 <pi-provider-entry> record。
;;
;; 语法
;; ----
;; (pi-load-provider-entry config-dir p-id) -> pi-provider-entry? | #f

;;
;; 参数
;; ----
;; entry : pi-provider-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base)
  (scheme file)
  (liii check)
  (liii pi code config user-providers)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(let ((e (pi-load-provider-entry (string-append FIXTURE "/full") "kimi-coding")))
  (check-true (pi-provider-entry? e))
  (check (pi-provider-entry-name e) => "Kimi Coding")
  (check (pi-provider-entry-base-url e) => "https://api.kimi.com/coding/v1")
) ;let

(check-false (pi-load-provider-entry (string-append FIXTURE "/full") "google"))

(check-report)
