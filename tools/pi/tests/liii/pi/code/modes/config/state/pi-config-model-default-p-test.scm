;; pi-config-model-default?
;; 判断模型是否为默认模型。
;;
;; 语法
;; ----
;; (pi-config-model-default? m)
;;
;; 参数
;; ----
;; m : alist
;; 模型条目。
;;
;; 返回值
;; ----
;; boolean
;; 是默认模型时返回 #t，否则返回 #f。

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
  (pi-config-model-default? (car (pi-config-provider-models (prov "kimi-coding")))
  ) ;pi-config-model-default?
  =>
  #t
) ;check

(check-report)
