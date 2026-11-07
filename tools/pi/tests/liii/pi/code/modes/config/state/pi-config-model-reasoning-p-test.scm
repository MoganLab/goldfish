;; pi-config-model-reasoning?
;; 判断模型是否支持推理模式。
;;
;; 语法
;; ----
;; (pi-config-model-reasoning? m)
;;
;; 参数
;; ----
;; m : alist
;; 模型条目。
;;
;; 返回值
;; ----
;; boolean
;; 支持推理模式时返回 #t，否则返回 #f。

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

(check
  (pi-config-model-reasoning? (car (pi-config-provider-models (prov "custom-kimi")))
  ) ;pi-config-model-reasoning?
  =>
  #t
) ;check

(check-report)
