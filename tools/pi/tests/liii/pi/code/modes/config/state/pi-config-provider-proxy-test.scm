;; pi-config-provider-proxy
;; 读取 provider 条目的生效代理地址。
;;
;; 语法
;; ----
;; (pi-config-provider-proxy p)
;;
;; 参数
;; ----
;; p : alist
;; provider 条目。
;;
;; 返回值
;; ----
;; (or string #f)
;; 代理地址，未配置时为 #f。

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

(check (pi-config-provider-proxy (prov "custom-kimi"))
  =>
  "http://provider-proxy:1080"
) ;check
(check (pi-config-provider-proxy (prov "kimi-coding"))
  =>
  "http://global-proxy:8080"
) ;check

(check-report)
