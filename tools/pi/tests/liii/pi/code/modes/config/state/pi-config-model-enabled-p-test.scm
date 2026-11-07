;; pi-config-model-enabled?
;; 判断模型是否可用。
;;
;; 语法
;; ----
;; (pi-config-model-enabled? m)
;;
;; 参数
;; ----
;; m : alist
;; 模型条目。
;;
;; 返回值
;; ----
;; boolean
;; 可用时返回 #t，否则返回 #f。

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
  (check (pi-config-model-enabled? (car ms)) => #t)
  (check
    (length (filter (lambda (m) (pi-config-model-enabled? m)) ms))
    =>
    1
  ) ;check
) ;let*

(check-report)
