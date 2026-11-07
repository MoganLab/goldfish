;; pi-config-provider-default?
;; 判断 provider 是否为默认 provider。
;;
;; 语法
;; ----
;; (pi-config-provider-default? p)
;;
;; 参数
;; ----
;; p : alist
;; provider 条目。
;;
;; 返回值
;; ----
;; boolean
;; 是默认 provider 时返回 #t，否则返回 #f。

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

(check (pi-config-provider-default? (prov "kimi-coding")) => #t)
(check (pi-config-provider-default? (prov "google")) => #f)

(check-report)
