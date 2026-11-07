;; pi-config-state-default-provider
;; 读取配置视图记录中的默认 provider。
;;
;; 语法
;; ----
;; (pi-config-state-default-provider st)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图记录。
;;
;; 返回值
;; ----
;; (or string #f)
;; 默认 provider 标识，未设置时为 #f。

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

(check (pi-config-state-default-provider st) => "kimi-coding")

(check-report)
