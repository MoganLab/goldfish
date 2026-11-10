;; pi-config-state-provider-count
;; 统计配置视图记录中的 provider 数量。
;;
;; 语法
;; ----
;; (pi-config-state-provider-count st)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图记录。
;;
;; 返回值
;; ----
;; integer
;; provider 数量。

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

(check (pi-config-state-provider-count st) => 8)

(check-report)
