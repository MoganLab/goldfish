;; pi-config-provider-key-source
;; 读取 provider 条目的密钥来源标记。
;;
;; 语法
;; ----
;; (pi-config-provider-key-source p)
;;
;; 参数
;; ----
;; p : alist
;; provider 条目。
;;
;; 返回值
;; ----
;; (or string #f)
;; 密钥来源标记（如环境变量名或 auth.json）。

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

(check (pi-config-provider-key-source (prov "kimi-coding")) => "auth.json")
(check (pi-config-provider-key-source (prov "google")) => #f)

(check-report)
