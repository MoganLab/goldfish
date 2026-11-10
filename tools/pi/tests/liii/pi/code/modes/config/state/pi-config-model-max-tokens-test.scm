;; pi-config-model-max-tokens
;; 读取模型条目的最大输出 token 数。
;;
;; 语法
;; ----
;; (pi-config-model-max-tokens m)
;;
;; 参数
;; ----
;; m : alist
;; 模型条目。
;;
;; 返回值
;; ----
;; (or number #f)
;; 最大输出 token 数。

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
  (pi-config-model-max-tokens (car (pi-config-provider-models (prov "kimi-coding")))
  ) ;pi-config-model-max-tokens
  =>
  131072
) ;check

(check-report)
