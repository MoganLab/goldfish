;; pi-config-provider
;; 按标识查找配置视图中的 provider 条目。
;;
;; 语法
;; ----
;; (pi-config-provider st p-id)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图记录。
;;
;; p-id : string
;; provider 标识。
;;
;; 返回值
;; ----
;; (or alist #f)
;; 匹配的 provider 条目，未找到时返回 #f。

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

(check (pi-config-provider-id (prov "google")) => "google")
(check (pi-config-provider st "missing") => #f)

(check-report)
