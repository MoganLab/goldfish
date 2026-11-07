;; pi-config-provider-models
;; 读取 provider 条目下的模型列表。
;;
;; 语法
;; ----
;; (pi-config-provider-models p)
;;
;; 参数
;; ----
;; p : alist
;; provider 条目。
;;
;; 返回值
;; ----
;; list
;; 模型条目列表。

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

(let ((ms (pi-config-provider-models (prov "kimi-coding"))))
  (check (length ms) => 5)
  (check (pi-config-model-id (car ms)) => "k3.1-pro")
) ;let*

(check-report)
