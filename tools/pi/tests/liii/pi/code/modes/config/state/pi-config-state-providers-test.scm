;; pi-config-state-providers
;; 读取配置视图记录中的 provider 列表。
;;
;; 语法
;; ----
;; (pi-config-state-providers st)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图记录。
;;
;; 返回值
;; ----
;; list
;; provider 条目列表。

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

(let ((providers (pi-config-state-providers st)))
  (check (> (length providers) 0) => #t)
  (check (pi-config-provider-id (car providers)) => "kimi-coding")
) ;let

(check-report)
