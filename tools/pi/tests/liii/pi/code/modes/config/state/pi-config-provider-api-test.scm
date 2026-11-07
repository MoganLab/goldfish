;; pi-config-provider-api
;; 读取 provider 条目的 API 协议类型。
;;
;; 语法
;; ----
;; (pi-config-provider-api p)
;;
;; 参数
;; ----
;; p : alist
;; provider 条目。
;;
;; 返回值
;; ----
;; (or string #f)
;; API 协议类型。

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

(check (pi-config-provider-api (prov "kimi-coding")) => "openai-completions")
(check (pi-config-provider-api (prov "custom-kimi")) => "anthropic-messages")

(check-report)
