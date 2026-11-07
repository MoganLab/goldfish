;; pi-config-state?
;; 判断对象是否为配置视图记录。
;;
;; 语法
;; ----
;; (pi-config-state? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是配置视图记录时返回 #t，否则返回 #f。

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

(check-true (pi-config-state? st))
(check-false (pi-config-state? #f))
(check-false (pi-config-state? '()))
(check-false (pi-config-state? (list (cons 'providers '()))))

(check-report)
