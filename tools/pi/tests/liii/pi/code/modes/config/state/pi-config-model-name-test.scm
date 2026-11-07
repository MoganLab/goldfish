;; pi-config-model-name
;; 读取模型条目的显示名称。
;;
;; 语法
;; ----
;; (pi-config-model-name m)
;;
;; 参数
;; ----
;; m : alist
;; 模型条目。
;;
;; 返回值
;; ----
;; (or string #f)
;; 模型显示名称。

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
  (pi-config-model-name (car (pi-config-provider-models (prov "kimi-coding"))))
  =>
  "Kimi K3.1 Pro"
) ;check

(check-report)
