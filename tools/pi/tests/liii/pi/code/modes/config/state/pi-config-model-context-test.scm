;; pi-config-model-context
;; 读取模型条目的上下文窗口大小。
;;
;; 语法
;; ----
;; (pi-config-model-context m)
;;
;; 参数
;; ----
;; m : alist
;; 模型条目。
;;
;; 返回值
;; ----
;; (or number #f)
;; 上下文窗口 token 数。

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
  (pi-config-model-context (car (pi-config-provider-models (prov "kimi-coding"))))
  =>
  1048576
) ;check

(check-report)
