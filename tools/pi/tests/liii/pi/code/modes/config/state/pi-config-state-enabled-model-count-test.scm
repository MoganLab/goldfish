;; pi-config-state-enabled-model-count
;; 统计配置视图记录中可用的模型数量。
;;
;; 语法
;; ----
;; (pi-config-state-enabled-model-count st)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图记录。
;;
;; 返回值
;; ----
;; integer
;; 可用模型数量。

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

(check (pi-config-state-enabled-model-count st)
  =>
  (+ 2 (length (pi-config-provider-models (prov "google"))))
) ;check

(check-report)
