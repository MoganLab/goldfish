;; pi-config-provider-base-url
;; 读取 provider 条目的基础 URL。
;;
;; 语法
;; ----
;; (pi-config-provider-base-url p)
;;
;; 参数
;; ----
;; p : alist
;; provider 条目。
;;
;; 返回值
;; ----
;; (or string #f)
;; 基础 URL。

(import (scheme base)
  (liii check)
  (liii list)
  (liii path)
  (liii pi code modes config state)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define DIR (string-append FIXTURE "/config-tui"))

(define st (pi-config-state DIR :env '()))

(define (prov id)
  (pi-config-provider st id)
) ;define

(check (pi-config-provider-base-url (prov "kimi-coding"))
  =>
  "https://api.kimi.com/coding/v1"
) ;check

(check-report)
